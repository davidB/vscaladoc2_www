package net_alchim31_vscaladoc2_www.snippet

import scala.xml.Null
import scala.xml.UnprefixedAttribute
import net.liftweb.http.RequestVar
import net.liftweb.http.{LiftScreen, S}
import _root_.scala.xml.{NodeSeq, Text, Elem, MetaData}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import Helpers._
import scala.util.matching.Regex

//prefill from field if user logged
object Contact extends LiftScreen {
  val name = VField(S ? "name", "",
                     List(trim),
                     List(Required("required"), MaxLength(120))
             )
  val email = VField(S ? "email", "",
                     List(trim),
                     List(Required("required"), MaxLength(120), Email)
             )
  val subject = VField(S ? "subject", "",
                     List(trim),
                     List(Required("required"), MaxLength(120))
             )
  val message = new VField(S ? "message", "",
                     List(trim),
                     List(Required("required"), MaxLength(2048))
             ) {
    override def toForm: Box[NodeSeq] = super.toForm.map {
    	_.map { node =>
    	  node match {
    	 	  case e : Elem if e.label == "input" => Elem(null, "textarea", new UnprefixedAttribute("rows", "20", new UnprefixedAttribute("cols", "80", e.attributes)), e.scope, e.child : _*)
    	 	  case x => x
    	  }
    	}
    }
  }

//  val subject extends RequestVar("")
//  object message extends RequestVar("")

  def finish() {
    import net.liftweb.util.Mailer
    import Mailer._
    println("message :" + message)
   // Mailer.sendMail(From(name.is + " <"+email.is+">"), Subject("[vscaladoc] " + subject.is), To("david.bernard@alchim31.net"), PlainMailBodyType(message.is))
  //  S.notice("I like "+flavor.is+" too!")
  }

  case class VField[T](
		  override val name : String,
		  override val default : T,
		  filters : List[T => T],
		  validators : List[Validation[T]]
		  )
		  (override implicit val manifest: Manifest[T]) extends Field {
	  type ValueType = T

    override def setFilter = filters
    override lazy val uniqueFieldId: Box[String] = Full("I"+randomString(15))
    override def validate = validators.flatMap(_.validate(this)(is))
    override def toForm: Box[NodeSeq] = super.toForm.map { xhtml =>
      validators.foldLeft(xhtml){ (f, x) => x.plugIn(f) }
    }
	/** override for compatibility, VField use validators internaly */
    override def validations = validators.map( _.validate(this) _)
  }

}
//  private
//  val emailPattern = """^[A-z0-9._%+-]+@(?:[A-z0-9-]+\.)+[a-z]{2,4}$"""
//
//  object name extends RequestVar("")
//  object email extends RequestVar("")
//  object subject extends RequestVar("")
//  object message extends RequestVar("")
//
//  def send(xhtml : NodeSeq): NodeSeq = {
//
//    def processSend() {
//      import net.liftweb.util.Mailer
//      import Mailer._
//
//      if ()
//      Mailer.sendMail(From(name.is + " <"+email.is+">"), Subject("[vscaladoc] " + subject.is), To("david.bernard@alchim31.net"), PlainMailBodyType(message.is))
//      //redirectTo(...)
//    }
//
//    bind("b", xhtml,
//       "name" -> SHtml.text(name.is, name(_), "placeholder"->"Full name", "required"->"true", "maxlength"->"60"),
//       "email" -> SHtml.text(email.is, email(_),  "placeholder"->"e.g. ryan@example.net", "title"->"Please enter a valid email", "required"->"true", "type"->"email", "pattern"->emailPattern, "maxlength" ->"120"),
//       "subject" -> SHtml.text(subject.is, subject(_),  "required" -> "true",  "maxlength" ->"120"),
//       "message" -> SHtml.textarea(message.is, message(_), "col" -> "80", "rows" -> "80",  "wrap"->"off", "required" ->"true"),
//       "submit" -> SHtml.submitButton(processSend))
//  }
//}
//

trait Validation[T] {
  def validate(fieldId : FieldIdentifier)( v : T) : List[FieldError] = validate(v) match {
	  case true => Nil
	  case false => List(FieldError(fieldId, S ? (errorMsg)))
  }
  def plugIn(xhtml : NodeSeq) : NodeSeq = xhtml

  def errorMsg : String
  def validate(v : T) : Boolean
}

object ValidationHelper {
  def plugIn(xhtml : NodeSeq, elemLabel : String, attrs : MetaData) : NodeSeq = {
    xhtml.map { x =>
  	  x match {
	    case x1 : Elem if x1.label == elemLabel => x1 % attrs
	    case _ => x
	  }
  	}
  }
}

case class Required(errorMsg : String = "required") extends Validation[String] {
  def validate(v : String) = v != null && v != ""
  override def plugIn(xhtml : NodeSeq) : NodeSeq = ValidationHelper.plugIn(xhtml, "input", new UnprefixedAttribute("required", "true", Null))
}

class RegExp(val regex : Regex, val errorMsg : String = "doesn't match format") extends Validation[String] {
  def validate(v : String) = regex.pattern.matcher(v).matches()
  override def plugIn(xhtml : NodeSeq) : NodeSeq = ValidationHelper.plugIn(xhtml, "input", new UnprefixedAttribute("pattern", regex.pattern.pattern, Null))
}

case object Email extends RegExp("""^[A-z0-9._%+-]+@(?:[A-z0-9-]+\.)+[a-z]{2,4}$""".r, "invalid.email.address") {
  override def plugIn(xhtml : NodeSeq) : NodeSeq = ValidationHelper.plugIn(super.plugIn(xhtml), "input", new UnprefixedAttribute("type", "email", Null))
}

case class MaxLength(max : Int, errorMsg : String = "too long") extends Validation[String] {
  def validate(v : String) = v != null && v.length <= max
  override def plugIn(xhtml : NodeSeq) : NodeSeq = ValidationHelper.plugIn(xhtml, "input", new UnprefixedAttribute("maxlength", max.toString, Null))
}
