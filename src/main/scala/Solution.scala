import util.Pixel
import util.Util

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val image_without_p3 = image.drop(3)

    val (dimensions, image_without_dimensions) = image_without_p3.splitAt(image_without_p3.indexOf('\n'))
    val (length, height) = dimensions.splitAt(dimensions.indexOf(' '))
    val only_pixels_img = image_without_dimensions.drop(5)

    val intLength = length.mkString.toInt
    val intHeight = height.drop(1).mkString.toInt

    @tailrec
    def organizePixels(pixelImage: List[Char], accRow: List[Pixel], acc: Image, length: Integer, height: Integer): Image = {
      if (height == 0) {
        acc.reverse
      } else {
        val (red, restList1) = pixelImage.splitAt(pixelImage.indexOf(' '))
        val restList2 = restList1.drop(1)
        val (green, restList3) = restList2.splitAt(restList2.indexOf(' '))
        val restList4 = restList3.drop(1)
        val (blue, restList5) = restList4.splitAt(restList4.indexOf('\n'))

        val new_pixel = Pixel(red.mkString.toInt, green.mkString.toInt, blue.mkString.toInt)

        if (length - 1 == 0) {
          organizePixels(restList5.drop(1), Nil, (new_pixel :: accRow).reverse :: acc, intLength, height - 1)
        } else {
          organizePixels(restList5.drop(1), new_pixel :: accRow, acc, length - 1, height)
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

    val final_image = header_image ++ image.flatten.flatMap(pixel => decomposePixel(pixel))

    final_image
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val row_pairs_list = image1.zip(image2)

    row_pairs_list.map(list_pair => list_pair._1 ++ list_pair._2)
  }

  // ex 3
  @tailrec
  def rotate(image: Image, degrees: Integer): Image = {
    val zero: Integer = 0
    degrees match {
      case `zero` => image
      case _ => rotate(image.transpose.reverse, degrees - 90)
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayscaledImage = image.map(row => row.map(pixel => Util.toGrayScale(pixel)))

    val convolutionWithKernel = applyConvolution(grayscaledImage, gaussianBlurKernel)

    val Mx = applyConvolution(convolutionWithKernel, Gx)
    val My = applyConvolution(convolutionWithKernel, Gy)

    val combined_Ms = Mx.zip(My).map(rows_pair => rows_pair._1.zip(rows_pair._2).map(elem_row => elem_row._1.abs + elem_row._2.abs))

    val final_image = combined_Ms.map(pixel_list => pixel_list.map(pixel => if (pixel < threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))

    final_image
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbours = Util.getNeighbors(image, kernel.length / 2)

    val image_after_convolution = neighbours.map(neighbours_list => neighbours_list.map(neighbour => neighbour.zip(kernel)
      .map(rows_pair => rows_pair._1.zip(rows_pair._2).foldLeft(0.0)((acc_row, row_corresponding_elems) => acc_row + row_corresponding_elems._1 * row_corresponding_elems._2))
      .foldLeft(0.0)((acc_convolution, row_convolution_result) => acc_convolution + row_convolution_result)))

    image_after_convolution
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    @tailrec
    def buildImage(image_modulos: List[List[Integer]], row: List[Integer], n: Integer, k: Integer): List[List[Integer]] = {
      val zero: Integer = 0
      n match {
        case `size` => image_modulos.reverse
        case _ => k match {
          case `size` => buildImage(row.reverse :: image_modulos, Nil, n + 1, 0)
          case `zero` => buildImage(image_modulos, (1 % m) :: row, n, k + 1)
          case `n` => buildImage(image_modulos, (1 % m) :: row, n, k + 1)
          case value => if (value > n) buildImage(image_modulos, (-1) :: row, n, k + 1)
          else buildImage(image_modulos, ((image_modulos.head.apply(k) + image_modulos.head.apply(k - 1)) % m) :: row, n, k + 1)
        }
      }
    }

    val matrix_modulos = buildImage(Nil, Nil, 0, 0)
    matrix_modulos.map(row_matrix => row_matrix.map(modulo_result => funct(modulo_result)))
  }
}
