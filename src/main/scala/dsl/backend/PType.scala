package dsl.backend

/**
  * Created by guillerminacledou on 2020-01-08
  */


sealed abstract class PType

case object In extends PType
case object Out extends PType
case object Mix extends PType