import util.Pixel

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites

  def fromStringPPM(image: List[Char]): Image = {
    val image_without_p3 = image.drop(3)

    val dimensions = image_without_p3.take(3)
    val (length, height) = dimensions.splitAt(dimensions.indexOf(' '))

    val image_without_dimensions = image_without_p3.drop(5)
    val only_pixels_img = image_without_dimensions.drop(3)

    val intLength = length.mkString.toInt
    val intHeight = height.drop(1).mkString.toInt

    def organizePixels(pixelImage: List[Char], accRow: List[Pixel], acc: Image, length: Integer, height: Integer): Image = {
      if(height == 0) {
        acc.reverse
      } else {
        val (red, restList1) = pixelImage.splitAt(pixelImage.indexOf(' '))
        val restList2 = restList1.drop(1)
        val (green, restList3) = restList2.splitAt(restList2.indexOf(' '))
        val restList4 = restList3.drop(1)
        val (blue, restList5) = restList4.splitAt(restList4.indexOf('\n'))

        val new_pixel = Pixel(red.mkString.toInt, green.mkString.toInt, blue.mkString.toInt)

        val auxAccRow = new_pixel :: accRow

        if (length - 1 == 0) {
          organizePixels(restList5.drop(1), Nil, auxAccRow.reverse :: acc, intLength, height - 1)
        } else {
          organizePixels(restList5.drop(1), auxAccRow, acc, length - 1, height)
        }
      }

    }

    organizePixels(only_pixels_img, Nil, Nil, intLength, intHeight)
  }

  def toStringPPM(image: Image): List[Char] = {
    val height = image.length
    val length = image.head.length

    def decomposePixel(pixel: Pixel) = List(pixel.red.toString, " ", pixel.green.toString, " ", pixel.blue.toString, "\n").flatten
    val header_image = List("P3\n", length.toString, " ", height.toString, "\n", 255.toString, "\n").flatten

    val image_flattened = image.flatten.flatMap(pixel => decomposePixel(pixel))

    val final_image = header_image ++ image_flattened

    final_image
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val zipped_images = image1 zip image2

    zipped_images.map(list_pair => list_pair._1 ++ list_pair._2)
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    degrees match
    case 0 => image
    case _ => rotate(image.transpose.reverse, degrees - 90)
  }

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
