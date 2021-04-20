package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait InfoFlag extends SMTLibFormatter

case object AllStatisticsFlag extends InfoFlag {
  override def format: String = ":all-statistics"
}

case object AssertionStackLevelsFlag extends InfoFlag {
  override def format: String = ":assertion-stack-levels"
}

case object AuthorsFlag extends InfoFlag {
  override def format: String = ":authors"
}

case object ErrorBehaviorFlag extends InfoFlag {
  override def format: String = ":error-behavior"
}

case object NameFlag extends InfoFlag {
  override def format: String = ":name"
}

case object ReasonUnknownFlag extends InfoFlag {
  override def format: String = ":reason-unknown"
}

case object VersionFlag extends InfoFlag {
  override def format: String = ":version"
}