package smtlib.syntax

import smtlib.SMTLibFormatter

trait Attribute extends SMTLibFormatter

case class KeyValueAttribute(key: Keyword, value: AttributeValue) extends Attribute {
  override def format(): String = s"${key.format()} ${value.format()}"
}

trait AttributeValue extends SMTLibFormatter

case class AttributeValues(values: Seq[AttributeValue]) extends AttributeValue {
  override def format(): String = s"(${SMTLibFormatter.format(values)})"
}
