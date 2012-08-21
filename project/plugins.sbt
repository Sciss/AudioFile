resolvers ++= Seq(
   "less is" at "http://repo.lessis.me",
   "coda" at "http://repo.codahale.com"
)

addSbtPlugin( "me.lessis" % "ls-sbt" % "0.1.2" )

addSbtPlugin( "com.eed3si9n" % "sbt-buildinfo" % "0.2.0" )  // provides version information to copy into main class

