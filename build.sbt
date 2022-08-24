ThisBuild / shellPrompt := ((s: State) => Project.extract(s).currentRef.project + "> ")

lazy val root    = Build.root
lazy val coreJVM = Build.coreJVM
lazy val coreJS  = Build.coreJS
lazy val webapp  = Build.webapp
