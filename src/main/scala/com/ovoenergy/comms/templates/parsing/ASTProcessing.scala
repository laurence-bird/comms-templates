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
      ast => insert(ast, root)
    }
    root._convert.map(validObj => validObj.fields)
  }

  private def insertObj(name: String, parent: Type): Obj = {
    val parentObj: Obj = parent match {
      case obj: Obj => obj
      case StrOrObj(replaceWith) =>
        // since we're trying to insert an object as a child, the parent must be an object so we upgrade it
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
        replaceWith(freshObj)
        freshObj
      case Str(markAsConflict) =>
        // We're trying to add a child object to a string. Mark as a conflict and return a dummy object so we can continue
        markAsConflict()
        Obj.fresh(markAsConflict)
      case Conflict =>
        Obj.fresh(markAsConflict = () => ())
    }
    parentObj.get(name) match {
      case None =>
        val obj = Obj.fresh(markAsConflict = () => parentObj.put(name, Conflict))
        parentObj.put(name, obj)
        obj
      case Some(existingObj: Obj) =>
        // nothing to do
        existingObj
      case Some(StrOrObj(replaceWith)) =>
        // upgrade to an object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
        replaceWith(freshObj)
        freshObj
      case Some(Str(_)) | Some(Loop(_)) | Some(Opt(Str(_))) | Some(Opt(Conflict)) =>
        // replace with a Conflict and return a dummy Obj so we can continue
        parentObj.put(name, Conflict)
        Obj.fresh(() => ())
      case Some(Opt(existingObj: Obj)) =>
        // nothing to do
        existingObj
      case Some(Opt(StrOrObj(replaceWith))) =>
        // upgrade to an object
        val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
        replaceWith(freshObj)
        freshObj
      case Some(Conflict) =>
        // nothing to do, just return a dummy Obj
        Obj.fresh(() => ())
    }
  }

  private def walkTree(rawKey: String, localRoot: Type)(implicit ctx: Context): (DottedKey, Type) = {
    val key = DottedKey.parse(rawKey)
    val (normalisedKey, correctRoot) = chooseRoot(key, localRoot)
    val parentNode = normalisedKey.init.foldLeft(correctRoot){
      case (parent, name) => insertObj(name, parent)
    }
    (key, parentNode)
  }

  private def chooseRoot(key: DottedKey, localRoot: Type)(implicit ctx: Context): (DottedKey, Type) = {
    if (key.init.headOption.contains("this") || (key.init.isEmpty && key.last == "this"))
      (DottedKey(key.init.drop(1), key.last), localRoot)
    else
      (key, ctx.root)
  }

  private def insert(ast: HandlebarsAST, localRoot: Type)(implicit ctx: Context): Unit = ast match {
    case Ref(k) =>
      val (key, parentNode) = walkTree(k, localRoot)
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
            val str = Str(markAsConflict = () => replaceWith(Conflict))
            replaceWith(str)
          } else {
            // it must be an object
            val obj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
            obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict)))
            replaceWith(obj)
          }
        case obj@Obj(_, markAsConflict) =>
          if (key.last == "this") {
            markAsConflict()
          } else {
            obj.get(key.last) match {
              case Some(Str(_)) => // already inserted, nothing to do
              case None | Some(StrOrObj(_)) => obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict)))
              case Some(Opt(StrOrObj(replaceWith))) => replaceWith(Str(markAsConflict = () => replaceWith(Conflict)))
              case _ =>
                obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
            }
          }
        case Conflict => // nothing to do
      }
    case Each(k, children, elseChildren) =>
      val (key, parentNode) = walkTree(k, localRoot)
      parentNode match {
        case obj: Obj =>
          obj.get(key.last) match {
            case Some(Loop(elem)) =>
              children.foreach(insert(_, localRoot = elem))
              elseChildren.foreach(insert(_, localRoot = localRoot))
            case None =>
              val loop = Loop.strOrObj
              obj.put(key.last, loop)
              children.foreach(insert(_, localRoot = loop.x))
              elseChildren.foreach(insert(_, localRoot = localRoot))
            case _ =>
              obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
          }
        case _ => // TODO
      }
    case If(k, children, elseChildren) =>
      val (key, parentNode) = walkTree(k, localRoot)
      parentNode match {
        case obj: Obj =>
          obj.get(key.last) match {
            case Some(Opt(elem)) =>
              children.foreach(insert(_, localRoot = localRoot))
              elseChildren.foreach(insert(_, localRoot = localRoot))
            case None =>
              val opt = Opt.strOrObj
              obj.put(key.last, opt)
              children.foreach(insert(_, localRoot = localRoot))
              elseChildren.foreach(insert(_, localRoot = localRoot))
            case _ =>
              obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
          }
        case _ => // TODO
      }
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
