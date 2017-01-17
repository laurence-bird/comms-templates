package com.ovoenergy.comms.templates.parsing

import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.RequiredTemplateData

private[parsing] object ASTProcessing {
  import IntermediateAST._
  import HandlebarsAST._

  private case class Context(root: Obj)

  def buildRequiredTemplateData(asts: Seq[HandlebarsAST]): ErrorsOr[Map[String, RequiredTemplateData]] = {
    val root = Obj.fresh(markAsConflict = () => ())
    implicit val context = Context(root)
    asts.foreach {
      ast => processAST(ast, root)
    }
    root._convert.map(validObj => validObj.fields)
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
    */
  private def insertObj(name: String, path: String, parent: Type): Obj = {
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
    parentObj.get(name) match {
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
    * @return The final node that was created
    */
  private def walkTree(rawKey: String, localRoot: Type)(implicit ctx: Context): (DottedKey, Type) = {
    val key = DottedKey.parse(rawKey)
    val (normalisedKey, correctRoot) = chooseRoot(key, localRoot)
    val (parentNode, _) = normalisedKey.init.foldLeft((correctRoot, Vector.empty[String])){
      case ((parent, parentPath), name) =>
        val path = parentPath :+ name
        val nextParent = insertObj(name, path.mkString("."), parent)
        (nextParent, path)
    }
    (key, parentNode)
  }

  /**
    * Choose whether to walk the tree starting at the global root or the local root.
    *
    * If the key starts with "this", it's scoped to the "this" context, so we should
    * strip the "this" and start walking from the local root.
    */
  private def chooseRoot(key: DottedKey, localRoot: Type)(implicit ctx: Context): (DottedKey, Type) = {
    if (key.init.headOption.contains("this") || (key.init.isEmpty && key.last == "this"))
      (DottedKey(key.init.drop(1), key.last), localRoot)
    else
      (key, ctx.root)
  }

  private def processRef(ref: Ref, localRoot: Type)(implicit ctx: Context): Unit = {
    val rawKey = ref.text
    val (key, parentNode) = walkTree(rawKey, localRoot)
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
    val rawKey = each.text
    val (key, parentNode) = walkTree(rawKey, localRoot)
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
      case StrOrObj(replaceWith) =>
        // upgrade to object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict(rawKey)))
        replaceWith(freshObj)

        val loop = Loop.strOrObj
        freshObj.put(key.last, loop)
        each.children.foreach(processAST(_, localRoot = loop.x))
        each.elseChildren.foreach(processAST(_, localRoot = localRoot))

      case _ =>
        println("yo I'm in your unhandled case (each)")
        // TODO is this case ever used?
    }
  }

  private def processIf(_if: If, localRoot: Type)(implicit ctx: Context): Unit = {
    val rawKey = _if.text
    val (key, parentNode) = walkTree(rawKey, localRoot)
    parentNode match {
      case obj: Obj =>
        obj.get(key.last) match {
          case Some(Opt(elem)) =>
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
