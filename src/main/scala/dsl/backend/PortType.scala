package dsl.backend

/**
  * Created by guillecledou on 2020-01-08
  */


sealed trait PortType {}

case object In extends PortType
case object Out extends PortType
case object Mix extends PortType