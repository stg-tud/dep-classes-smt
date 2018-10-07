package smtlib.syntax

import smtlib.SMTLibFormatter

trait InfoFlag extends SMTLibFormatter

case object AllStatistics extends InfoFlag {
  override def format(): String = ":all-statistics"
}

case object AssertionStackLevels extends InfoFlag {
  override def format(): String = ":assertion-stack-levels"
}

case object Authors extends InfoFlag {
  override def format(): String = ":authors"
}

case object ErrorBehavior extends InfoFlag {
  override def format(): String = ":error-behavior"
}

case object Name extends InfoFlag {
  override def format(): String = ":name"
}

case object ReasonUnknown extends InfoFlag {
  override def format(): String = ":reason-unknown"
}

case object Version extends InfoFlag {
  override def format(): String = ":version"
}