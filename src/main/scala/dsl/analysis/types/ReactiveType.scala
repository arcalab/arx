package dsl.analysis.types

/**
  * Created by guillerminacledou on 2020-01-21
  */


sealed trait ReactiveType
case object Push extends ReactiveType
case object Pull extends ReactiveType

