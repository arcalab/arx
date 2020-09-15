package dsl.common

/**
  * Created by guillerminacledou on 2019-06-05
  */


class TypeException(err:String) extends RuntimeException(err) {}//(implicit pos:Position)
  //extends RuntimeException(pos.line+": "+ err) {}
