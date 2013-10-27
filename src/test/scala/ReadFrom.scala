/**
 * Created with IntelliJ IDEA.
 * User: tomasz
 * Date: 27.10.13
 * Time: 20:44
 * To change this template use File | Settings | File Templates.
 */
trait ReadFrom {

  def readFrom(fileName: String) = scala.io.Source.fromFile(getClass.getResource(fileName).toURI).mkString

}
