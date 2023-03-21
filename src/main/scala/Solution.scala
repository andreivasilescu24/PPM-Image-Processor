import util.Pixel

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites

  def fromStringPPM(image: List[Char]): Int = {
    val image_without_p3 = image.drop(3)

    val dimensions = image_without_p3.take(3)

    val image_without_dimensions = image_without_p3.drop(5)
    val only_pixels_img = image_without_dimensions.drop(3)

    //    def organizePixels(pixelImage: List[Char], acc: List[List[Pixel]]): Image = {
    //      pixelImage match {
    //        case Nil => ???
    //        case x :: xs => if(x == ' ')
    //      }
    //    }

    val (x, y) = only_pixels_img.splitAt(only_pixels_img.indexOf(' '))
    println(x.mkString.toInt)
    println(y)
    //    println(dimensions)
    println(only_pixels_img)

    //    organizePixels(only_pixels_img, Nil: List[Char])
    return 0
  }

  def toStringPPM(image: Image): List[Char] = ???

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = ???

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = ???

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = ???

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = ???

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = ???

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = ???
}
