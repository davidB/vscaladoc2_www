package net_alchim31_vscaladoc2_www.model

import java.net.URL
import net.liftweb.common._
import _root_.net.liftweb.http._

sealed trait ApiProvider {
  def rurlPathOf(packageName: String, typeName: String, memberName: String, memberType64: String): Box[String] = Failure("not supported")
  def rurlPathOf(packageName: String, typeName: String, memberName: String): Box[String] = Failure("not supported")
  def rurlPathOf(packageName: String, typeName: String): Box[String] = Failure("not supported")
  def rurlPathOf(packageName: String): Box[String] = Failure("not supported")
  def rurlPathOf(): Box[String] = Failure("not supported")
  def rurlPathOf(entityPath: List[String]): Box[String] = {
    entityPath match {
      case packageName :: typeName :: memberName :: memberType64 :: Nil => {
        rurlPathOf(packageName, typeName, memberName, memberType64)
      }
      case packageName :: typeName :: memberName :: Nil => {
        rurlPathOf(packageName, typeName, memberName)
      }
      case packageName :: typeName :: Nil => {
        rurlPathOf(packageName, typeName)
      }
      case packageName :: Nil => {
        rurlPathOf(packageName)
      }
      case Nil => {
        rurlPathOf()
      }
      case _ => Failure("too many section (" + entityPath.size + ") in the path : " + entityPath)
    }
  }
}

case object Scaladoc extends ApiProvider {
  override def rurlPathOf(packageName: String): Box[String] = {
    Full(packageName.replace('.', '/') + "$content.html")
  }
}
case object Scaladoc2 extends ApiProvider
case object VScaladoc extends ApiProvider
case object VScaladoc2 extends ApiProvider {
  override def rurlPathOf(packageName: String, typeName: String, memberName: String, memberType64: String): Box[String] = {
    Full(packageName + "/" + typeName + "/" + memberName + "__" + memberType64 + ".json")
  }
  override def rurlPathOf(packageName: String, typeName: String, memberName: String): Box[String] = {
    Full(packageName + "/" + typeName + "/" + memberName + ".json")
  }
  override def rurlPathOf(packageName: String, typeName: String): Box[String] = {
    Full(packageName + "/" + typeName + ".json")
  }
  override def rurlPathOf(packageName: String): Box[String] = {
    Full(packageName + ".json")
  }
  override def rurlPathOf(): Box[String] = {
    Full("_overview.json")
  }
}

//in memberType64 use canonical type name
case object Javadoc2 extends ApiProvider {
  //TODO add support for args of memberType64 (remove generics, remove return type)
  override def rurlPathOf(packageName: String, typeName: String, memberName: String, memberType64: String): Box[String] = {
    Full(packageName.replace('.', '/') + "/" + typeName + ".html#" + memberName + "()")
  }
  override def rurlPathOf(packageName: String, typeName: String, memberName: String): Box[String] = {
    Full(packageName.replace('.', '/') + "/" + typeName + ".html#" + memberName + "()")
  }
  override def rurlPathOf(packageName: String, typeName: String): Box[String] = {
    Full(packageName.replace('.', '/') + "/" + typeName + ".html")
  }
  override def rurlPathOf(packageName: String): Box[String] = {
    Full(packageName.replace('.', '/') + "/package.html")
  }
  override def rurlPathOf(): Box[String] = {
    Full("overview.html")
  }
}

