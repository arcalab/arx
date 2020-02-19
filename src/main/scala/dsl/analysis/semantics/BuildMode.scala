package dsl.analysis.semantics

/**
  * Created by guillecledou on 2020-02-19
  */


/**
  * Ways of building an automaton for a stream builder
  * [[AllMode]]     -- external entities can do push/pull of streams at any time
  * [[PushMode]]    -- external entities can only do push of streams at any time
  * [[PullMode]]    -- external entities can only do pull of streams at any time
  * [[ClosedMode]]  -- external entities can not do push/pull of streams
  */
sealed trait BuildMode
object AllMode extends BuildMode
object PushMode extends BuildMode
object PullMode extends BuildMode
object ClosedMode extends BuildMode