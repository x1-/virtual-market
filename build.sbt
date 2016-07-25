organization  := "com.inkenkun.x1"

version       := "0.1"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "repo.typesafe.com" at "http://repo.typesafe.com/typesafe/releases/",
  "scalaz-bintray"    at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= {
  val akkaV = "2.4.5"
  val sprayV = "1.3.3"
  Seq(
    "io.spray"            %%  "spray-can"                    % sprayV,
    "io.spray"            %%  "spray-routing"                % sprayV,
    "io.spray"            %%  "spray-testkit"                % sprayV  % "test",
    "com.typesafe.akka"   %%  "akka-actor"                   % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"                 % akkaV   % "test",
    "org.json4s"          %%  "json4s-jackson"               % "3.3.0",
    "net.debasishg"       %%  "redisclient"                  % "3.0",
    "org.scalikejdbc"     %%  "scalikejdbc"                  % "2.4.2",
    "mysql"                %  "mysql-connector-java"         % "5.1.39",
    "org.apache.commons"   %  "commons-lang3"                % "3.3.2",
    "joda-time"            %  "joda-time"                    % "2.9.4",
    "com.google.apis"      %  "google-api-services-bigquery" % "v2-rev307-1.22.0",
    "org.specs2"          %%  "specs2-core"                  % "3.6.1-scalaz-7.0.6" % "test"
  )
}

