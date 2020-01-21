package dsl.backend

/**
  * Created by guillecledou on 2020-01-08
  */


sealed trait PType

case object In extends PType
case object Out extends PType
case object Mix extends PType