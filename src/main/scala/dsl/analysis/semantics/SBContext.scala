package dsl.analysis.semantics

import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry

/**
  * Created by guillecledou on 2020-02-06
  */


class SBContext {

  // a map from known functions names to their corresponding stream builders entries
  protected val functions:Map[String,StreamBuilderEntry] = Map()

  /**
    * Add a new function to the context.
    * @param name function name
    * @param sb stream builder entry
    * @return updated context with an entry from @name to @sb
    */
  def add(name:String,sb: StreamBuilderEntry):SBContext = {
    val old = functions
    new SBContext {
      override val functions: Map[String, StreamBuilderEntry] = old + (name->sb)
    }
  }

  /**
    * Get the stream builder for a given function.
    * Will produce an error if the function @name does not exists.
    * @param name function name
    * @return stream builder entry associated to function @name, if exists,
    *         or an exception, otherwise
    */
  def apply(name:String):StreamBuilderEntry = functions(name)

  /**
    * Checks if a function named @name exists in the context
    * @param name function name
    * @return whether the function exists in the context
    */
  def contains(name:String):Boolean = functions.contains(name)

  /**
    * Returns the entire context
    * @return context of stream builder
    */
  def get():Map[String,StreamBuilderEntry] = functions
}

object SBContext {

  /**
    * Creates a empty context for stream builder entries
    * @return
    */
  def apply():SBContext = new SBContext

  /**
    * Creates a context based on a given map of names to stream builder entries
    * @param ctx
    * @return
    */
  def apply(map:Map[String,StreamBuilderEntry]):SBContext = {
    val ctx = new SBContext {
      override val functions: Map[String, StreamBuilderEntry] = map
    }
    ctx
  }
}
