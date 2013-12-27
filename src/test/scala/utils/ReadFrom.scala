package utils

/**
 * Created with IntelliJ IDEA.
 * User: tomasz
 * Date: 27.10.13
 * Time: 20:44
 * To change this template use File | Settings | File Templates.
 */
trait ReadFrom {

  def readFrom(fileName: String) = scala.io.Source.fromFile(getClass.getClassLoader.getResource(fileName).toURI).mkString.trim.replaceAll("\n","")

  def readAsList(fileName: String) = scala.io.Source.fromFile(getClass.getClassLoader.getResource(fileName).toURI).mkString.split("\n").map(_.trim)

}
