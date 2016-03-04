package controllers

import java.io.FileWriter

import play.api.Play
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.collection.mutable
import scala.concurrent.Future


/**
  * Created by davi5017 on 3/3/2016.
  */
object Services extends Controller {

  val msgs = mutable.Set.empty[String]


  def faa_stream2_get(filename: Option[String]) = Action {


    if (msgs.size > 0) {
      // If filename specified then write to file
      filename match {
        case None => {}
        case Some(a) => {
          val fn = Play.current.configuration.getString("output.folder").get + s"$a"
          System.out.println(fn)

          val fw = new FileWriter(fn)
          for (msg <- msgs) {
            fw.write(msg + "\n")
          }
          fw.close()


        }

      }

      // Calculates statistics
//      var latencyArray = Array[Double]()

      var cnt = 0
//      var latencyAverage = 0.0
      var latencyMax = Double.MinValue
      var latencyMin = Double.MaxValue
      var latencySum = 0.0
      var latencySumSquared = 0.0

      var t0Max = Double.MinValue
      var t0Min = Double.MaxValue
      var t1Max = Double.MinValue
      var t1Min = Double.MaxValue

      for (msg <- msgs) {
        cnt += 1
        val parts = msg.split(",")
        val t0 = parts(1).toDouble
        val t1 = parts(2).toDouble
        //System.out.println(t1 - t0)
        val latency = (t1 - t0)
        latencySum += latency
        latencySumSquared += latency*latency
        //latencyArray = latencyArray :+ latency
//        if (cnt == 1) latencyAverage = latency.toDouble
//        else {latencyAverage = latencyAverage * (cnt-1)/cnt + latency/cnt}
        if (latency > latencyMax) latencyMax = latency
        if (latency < latencyMin) latencyMin = latency

        if (t0 > t0Max) t0Max = t0
        if (t0 < t0Min) t0Min = t0
        if (t1 > t1Max) t1Max = t1
        if (t1 < t1Min) t1Min = t1

      }

      val latencyAverage = latencySum/msgs.size.toDouble
      val latencyStdDev = Math.sqrt(cnt*latencySumSquared-latencySum*latencySum)/cnt

      val averageInput = msgs.size/(t0Max - t0Min)
      val averageOutput = msgs.size/(t1Max - t1Min)

      System.out.println("%11.2f".format(latencyAverage))
      System.out.println("%11.2f".format(latencyStdDev))
      System.out.println("%8.0f".format(latencyMin))
      System.out.println("%8.0f".format(latencyMax))
      System.out.println("%8.0f".format(averageInput*1000.0))
      System.out.println("%8.0f".format(averageOutput*1000.0))



      val json = Json.obj(
        "numMessages" -> msgs.size,
        "avgLatency" -> latencyAverage,
        "stdLatency" -> latencyStdDev,
        "minLatency" -> latencyMin,
        "maxLatency" -> latencyMax,
        "averageInput" -> averageInput*1000.0,
        "averageOutput" -> averageOutput*1000.0
      )

      msgs.clear()

      Ok(Json.prettyPrint(json))   // Return Content-Type: text/plain
      //Ok(json)  // Returns Content-Type: application/json

//      Ok(views.html.messages("%11.2f".format(latencyAverage))
//      ("%11.2f".format(latencyStdDev))
//      ("%8.0f".format(latencyMin))
//      ("%8.0f".format(latencyMax))
//      ("%8.0f".format(averageInput*1000.0))
//      ("%8.0f".format(averageOutput*1000.0))
//      )


    } else {
      msgs.clear()
      Ok(views.html.test("No Messages"))
    }


  }

  def faa_stream2 = Action.async(parse.json) { request =>

    // The following line if needed for handling the Future return for this async call
    import scala.concurrent.ExecutionContext.Implicits.global

    // Get the json from the request
    val json = request.body
    //System.out.println(json)

    // Read values from the Json; using Option helps avoid problems with missing fields
    val t0: Option[Long] = (json \ "t0").asOpt[Long]
    val id: Option[Int] = (json \ "id").asOpt[Int]
    //System.out.println(t0)

    // create the fields I want to save
    val idNumber = id.getOrElse(-1)
    val start = t0.getOrElse(-1)
    val end = System.currentTimeMillis()

    // Create output line
    val line = idNumber + "," + start + "," + end
    //System.out.println(line)
    msgs.add(line)

    //System.out.println(msgs.size)

    // Return a template.  Not really necessary in this case.  Added for learning purposes.
    Future(Ok(views.html.test("Post David")))
  }

}
