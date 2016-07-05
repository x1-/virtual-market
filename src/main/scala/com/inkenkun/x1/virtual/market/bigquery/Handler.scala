package com.inkenkun.x1.virtual.market.bigquery

import scala.collection.JavaConverters._

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.bigquery.{Bigquery, BigqueryScopes}
import com.google.api.services.bigquery.model.{QueryRequest,TableRow}

object Handler {

  val transport = new NetHttpTransport
  val jsonFactory = new JacksonFactory
  val credential = GoogleCredential.getApplicationDefault( transport, jsonFactory )

  val client = createClient

  def createClient: Bigquery = {

    val newCredential =
      if ( credential.createScopedRequired() )
        credential.createScoped(BigqueryScopes.all())
      else
        credential

    new Bigquery
      .Builder( transport, jsonFactory, credential )
      .setApplicationName( "virtual-market" ).build()
  }

  def executeQuery( query: String, projectId: String ): Seq[Seq[String]] = {
    val response = client.jobs.query(
      projectId,
      new QueryRequest().setQuery( query )
    ).execute()
    val queryResult = client.jobs.getQueryResults(
      response.getJobReference.getProjectId,
      response.getJobReference.getJobId
    ).execute()
    val rows = queryResult.getRows
    if ( rows == null )
      Seq.empty[Seq[String]]
    else
      rows.asScala.map { row =>
        row.getF.asScala.map { col =>
          col.getV.toString
        }
      }
  }

  def printResults( results: Seq[TableRow] ) = {
    println( "\nQuery Results:\n------------\n" )
    results.foreach { row =>
      row.getF.asScala.foreach { cell =>
        println( s"${cell.getV}" )
      }
    }
  }
}
