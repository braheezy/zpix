package main

import (
	"fmt"
	"image"
	_ "image/jpeg"
	"os"

	"github.com/hajimehoshi/ebiten/v2"
)

const scaleFactor = 0.5

type Game struct {
	img       *ebiten.Image
	imgWidth  int
	imgHeight int
}

func (g *Game) Update() error {
	return nil
}

func (g *Game) Draw(screen *ebiten.Image) {
	// Draw the image on the screen with scaling
	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(scaleFactor, scaleFactor)
	screen.DrawImage(g.img, op)
}

func (g *Game) Layout(outsideWidth, outsideHeight int) (int, int) {
	// Define the window size based on scaled image dimensions
	return int(float64(g.imgWidth) * scaleFactor), int(float64(g.imgHeight) * scaleFactor)
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: dump_jpeg <jpeg_file>")
		os.Exit(1)
	}

	// Open the JPEG file
	filePath := os.Args[1]
	file, err := os.Open(filePath)
	if err != nil {
		fmt.Printf("Failed to open file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	// Decode the image
	img, format, err := image.Decode(file)
	if err != nil {
		fmt.Printf("Failed to decode image: %v\n", err)
		os.Exit(1)
	}
	// Print metadata from Decode
	fmt.Println("Image Format:", format)
	fmt.Println("Color Model:", img.ColorModel())

	// Reset the file cursor to the beginning
	if _, err := file.Seek(0, 0); err != nil {
		fmt.Printf("Failed to reset file cursor: %v\n", err)
		os.Exit(1)
	}
	// DecodeConfig to get dimensions
	cfg, _, err := image.DecodeConfig(file)
	if err != nil {
		fmt.Printf("Failed to get image config: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Width:", cfg.Width)
	fmt.Println("Height:", cfg.Height)

	// Write raw pixel data to a file
	outputFile, err := os.Create("pixel_dump.go.raw")
	if err != nil {
		fmt.Printf("Failed to create output file: %v\n", err)
		os.Exit(1)
	}
	defer outputFile.Close()

	bounds := img.Bounds()
	width := bounds.Dx()
	height := bounds.Dy()

	// Iterate through pixels and write RGBA data
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			c := img.At(x, y)
			r, g, b, a := c.RGBA()
			// Normalize 16-bit RGBA to 8-bit per channel
			outputFile.Write([]byte{
				uint8(r >> 8), // Red
				uint8(g >> 8), // Green
				uint8(b >> 8), // Blue
				uint8(a >> 8), // Alpha
			})
		}
	}
	fmt.Println("Pixel data written to 'pixel_dump.go.raw'")

	// Convert image.Image to ebiten.Image
	ebitenImg := ebiten.NewImageFromImage(img)

	// Display image using Ebiten
	ebiten.SetWindowTitle("JPEG Viewer")
	ebiten.SetWindowSize(int(float64(width)*scaleFactor), int(float64(height)*scaleFactor))

	game := &Game{
		img:       ebitenImg,
		imgWidth:  width,
		imgHeight: height,
	}
	if err := ebiten.RunGame(game); err != nil {
		fmt.Printf("Failed to run Ebiten game: %v\n", err)
		os.Exit(1)
	}
}
