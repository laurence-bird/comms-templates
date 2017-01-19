package com.ovoenergy.comms.templates.parsing

import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.RequiredTemplateData

private[parsing] object ASTProcessing {
  import IntermediateAST._
  import HandlebarsAST._

  private case class Context(root: Obj)

  def buildRequiredTemplateData(asts: Seq[HandlebarsAST]): ErrorsOr[RequiredTemplateData.obj] = {
    val root = Obj.fresh(markAsConflict = () => ())
    implicit val context = Context(root)
    asts.foreach {
      ast => processAST(ast, root)
    }
    root._convert
  }

  /**
    * Try to make `parent`.`name` into an object.
    *
    * - If it's already an object, that's fine, nothing to do
    * - If it can be upgraded to an object, do so
    * - If it can't be upgraded to an object, mark it as a conflict
    *
    * In case of conflict, we create and return a dummy object.
    * Further recursive calls will add children to this object,
    * blissfully unaware that the object is an orphan, not part of the AST tree.
    *
    * e.g. if we try to create objects `foo`, `foo.bar` and `foo.bar.baz`, but `foo` is already a string,
    * we will:
    * 1. mark `foo` as a conflict in the tree and create a dummy `foo` object
    * 2. successfully add a `bar` object to the dummy `foo` object
    * 3. successfully add a `baz` object to the `foo.bar` object
    *
    * @param path The full key of the object being inserted, e.g. `foo.bar.baz`, only used for error messaging
    * @return The inserted (or already present) object and the new parent (which may have been replaced)
    */
  private def insertObj(name: String, path: String, parent: Type): (Obj, Type) = {
    val parentObj: Obj = parent match {
      case obj: Obj => obj
      case StrOrObj(replaceWith) =>
        // since we're trying to insert an object as a child, the parent must be an object so we upgrade it
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(path)))
        replaceWith(freshObj)
        freshObj
      case Str(markAsConflict) =>
        // We're trying to add a child object to a string. Mark as a conflict and return a dummy object so we can continue
        markAsConflict()
        Obj.fresh(markAsConflict)
      case Conflict(_) =>
        Obj.fresh(markAsConflict = () => ())
    }
    val insertedObj = parentObj.get(name) match {
      case None =>
        val obj = Obj.fresh(markAsConflict = () => parentObj.put(name, Conflict(path)))
        parentObj.put(name, obj)
        obj
      case Some(existingObj: Obj) =>
        // nothing to do
        existingObj
      case Some(StrOrObj(replaceWith)) =>
        // upgrade to an object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(path)))
        replaceWith(freshObj)
        freshObj
      case Some(Str(_)) | Some(Loop(_)) | Some(Opt(Str(_))) | Some(Opt(Conflict(_))) =>
        // replace with a Conflict and return a dummy Obj so we can continue
        parentObj.put(name, Conflict(path))
        Obj.fresh(() => ())
      case Some(Opt(existingObj: Obj)) =>
        // nothing to do
        existingObj
      case Some(Opt(StrOrObj(replaceWith))) =>
        // upgrade to an object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(path)))
        replaceWith(freshObj)
        freshObj
      case Some(Conflict(_)) =>
        // nothing to do, just return a dummy Obj
        Obj.fresh(() => ())
    }
    (insertedObj, parentObj)
  }

  /**
    * Split the key by dot, then walk down the tree, making everything an object along the way.
    *
    * e.g. if the key is "foo.bar.baz", we:
    * 1. Create an object "foo" under the root
    * 2. Create an object "bar" under "foo"
    * 3. Return the "bar" object as the final result.
    *
    * For each tree node along the way:
    * - If it's already an object, that's fine, nothing to do
    * - If it can be upgraded to an object, do so
    * - If it can't be upgraded to an object, mark it as a conflict
    *
    * Whether we walk down from the global root or the local root depends on the value of the key.
    * e.g. if the key is "foo.bar", we assume it references a top-level "foo" object, so we start
    * from the global root.
    *
    * On the other hand, if the key is "this.foo.bar", then the reference is scoped to "this", so
    * we start from the local root. (Note that the global root and the local root may be the same place).
    *
    * Note that walking the tree can cause the local root to be replaced, if it was a StrOrObj and it
    * got upgraded to an Obj. Thus we also return the new local root.
    *
    * @return The parsed key, the final node that was created, and the new local root (which may have been replaced)
    */
  private def walkTree(rawKey: String, localRoot: Type)(implicit ctx: Context): (DottedKey, Type, Type) = {
    val key = DottedKey.parse(rawKey)
    val (normalisedKey, startFromRoot, choseLocal) = chooseRoot(key, localRoot)
    val (parentNode, _, newRoot) = normalisedKey.init.foldLeft((startFromRoot, Vector.empty[String], startFromRoot)){
      case ((parent, parentPath, updatedRoot), name) =>
        val path = parentPath :+ name
        val (nextParent, updatedParent) = insertObj(name, path.mkString("."), parent)
        if (parentPath.isEmpty) {
          // this is the first time around, so the parent was the local root
          (nextParent, path, updatedParent)
        } else {
          // after that, just pass the updated local root along to the next iteration
          (nextParent, path, updatedRoot)
        }
    }
    val newLocalRoot = if (choseLocal) newRoot else localRoot
    (key, parentNode, newLocalRoot)
  }

  /**
    * Choose whether to walk the tree starting at the global root or the local root.
    *
    * If the key starts with "this", it's scoped to the "this" context, so we should
    * strip the "this" and start walking from the local root.
    */
  private def chooseRoot(key: DottedKey, localRoot: Type)(implicit ctx: Context): (DottedKey, Type, Boolean) = {
    if (key.init.headOption.contains("this") || (key.init.isEmpty && key.last == "this"))
      (DottedKey(key.init.drop(1), key.last), localRoot, true)
    else
      (key, ctx.root, false)
  }

  private def processRef(ref: Ref, localRoot: Type)(implicit ctx: Context): Unit = {
    val rawKey = ref.text
    val (key, parentNode, _) = walkTree(rawKey, localRoot)
    parentNode match {
      case Str(markAsConflict) =>
        if (key.last == "this") {
          // OK, nothing to do
        } else
          // we're trying to reference a child field of a string - conflict!
          markAsConflict()
      case StrOrObj(replaceWith) =>
        if (key.last == "this") {
          // it must be a string
          val str = Str(markAsConflict = () => replaceWith(Conflict(rawKey)))
          replaceWith(str)
        } else {
          // it must be an object
          val obj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(rawKey)))
          obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict(rawKey))))
          replaceWith(obj)
        }
      case obj@Obj(_, markAsConflict) =>
        if (key.last == "this") {
          markAsConflict()
        } else {
          obj.get(key.last) match {
            case Some(Str(_)) => // already inserted, nothing to do
            case None | Some(StrOrObj(_)) => obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict(rawKey))))
            case Some(Opt(StrOrObj(replaceWith))) => replaceWith(Str(markAsConflict = () => replaceWith(Conflict(rawKey))))
            case _ =>
              obj.put(key.last, Conflict(rawKey)) // conflict! the same thing has 2 different types
          }
        }
      case Conflict(_) => // nothing to do
    }
  }

  private def processEach(each: Each, localRoot: Type)(implicit ctx: Context): Unit = {
    val (key, parentNode, newLocalRoot) = walkTree(each.text, localRoot)
    _processEach(each, key, parentNode, newLocalRoot)
  }

  private def _processEach(each: Each, key: DottedKey, parentNode: Type, localRoot: Type)(implicit ctx: Context): Unit = {
    val rawKey = each.text
    parentNode match {
      case obj: Obj =>
        obj.get(key.last) match {
          case Some(Loop(elem)) =>
            each.children.foreach(processAST(_, localRoot = elem))
            each.elseChildren.foreach(processAST(_, localRoot = localRoot))
          case None =>
            val loop = Loop.strOrObj
            obj.put(key.last, loop)
            each.children.foreach(processAST(_, localRoot = loop.x))
            each.elseChildren.foreach(processAST(_, localRoot = localRoot))
          case _ =>
            obj.put(key.last, Conflict(rawKey)) // conflict! the same thing has 2 different types
        }
      case x @ StrOrObj(replaceWith) =>
        // upgrade to object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(rawKey)))
        replaceWith(freshObj)

        // we might have just replaced the local root with a new object
        val newLocalRoot = if (x eq localRoot) freshObj else localRoot

        val loop = Loop.strOrObj
        freshObj.put(key.last, loop)
        each.children.foreach(processAST(_, localRoot = loop.x))
        each.elseChildren.foreach(processAST(_, localRoot = newLocalRoot))

      case _ =>
        println("yo I'm in your unhandled case (each)")
      // TODO is this case ever used?
    }
  }

  private def processIf(_if: If, localRoot: Type)(implicit ctx: Context): Unit = {
    val (key, parentNode, newLocalRoot) = walkTree(_if.text, localRoot)
    _processIf(_if, key, parentNode, newLocalRoot)
  }

  private def _processIf(_if: If, key: DottedKey, parentNode: Type, localRoot: Type)(implicit ctx: Context): Unit = {
    val rawKey = _if.text
    parentNode match {
      case obj: Obj =>
        obj.get(key.last) match {
          case Some(Opt(_)) =>
            _if.children.foreach(processAST(_, localRoot = localRoot))
            _if.elseChildren.foreach(processAST(_, localRoot = localRoot))
          case None =>
            val opt = Opt.strOrObj
            obj.put(key.last, opt)
            _if.children.foreach(processAST(_, localRoot = localRoot))
            _if.elseChildren.foreach(processAST(_, localRoot = localRoot))
          case _ =>
            obj.put(key.last, Conflict(rawKey)) // conflict! the same thing has 2 different types
        }
      case x @ StrOrObj(replaceWith) =>
        // upgrade to object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(rawKey)))
        replaceWith(freshObj)

        // we might have just replaced the local root with a new object
        val newLocalRoot = if (x eq localRoot) freshObj else localRoot

        val opt = Opt.strOrObj
        freshObj.put(key.last, opt)
        _if.children.foreach(processAST(_, localRoot = newLocalRoot))
        _if.elseChildren.foreach(processAST(_, localRoot = newLocalRoot))
      case _ =>
        println("yo I'm in your unhandled case (if)")
        // TODO is this case ever used?
    }
  }

  /**
    * Update the intermediate AST tree to reflect the new information provided by the given AST.
    */
  private def processAST(ast: HandlebarsAST, localRoot: Type)(implicit ctx: Context): Unit = ast match {
    case ref: Ref => processRef(ref, localRoot)
    case each: Each => processEach(each, localRoot)
    case _if: If => processIf(_if, localRoot)
  }

}

object DottedKey {
  def parse(key: String): DottedKey = {
    val parts = key.split('.')
    val result = DottedKey(parts.dropRight(1), parts.last)
    result
  }
}
case class DottedKey(init: Seq[String], last: String)
