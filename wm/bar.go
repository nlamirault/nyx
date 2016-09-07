// Copyright (C) 2016  Nicolas Lamirault <nicolas.lamirault@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package wm

import (
	"bytes"
	"io/ioutil"
	"log"
	"time"

	"github.com/BurntSushi/freetype-go/freetype/truetype"
	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/xgraphics"
	"github.com/BurntSushi/xgbutil/xwindow"
	"github.com/martine/gocairo/cairo"
)

type Bar struct {
	x          *xgbutil.XUtil
	window     *xwindow.Window
	wm         *WindowManager
	workspaces *xwindow.Window
}

func NewBar(x *xgbutil.XUtil, wm *WindowManager) (*Bar, error) {
	log.Println("[DEBUG] Creating new bar")
	rg := xwindow.RootGeometry(x)
	win, err := xwindow.Generate(x)
	if err != nil {
		return nil, err
	}

	err = win.CreateChecked(x.RootWin(), 0, 0, rg.Width(), 20, xproto.CwBackPixel, 0x333333)
	if err != nil {
		return nil, err
	}

	runClock(x, win)

	bar := &Bar{x: x, window: win, wm: wm}
	bar.drawWorkspaces()

	return bar, nil
}

func (bar *Bar) Draw() {
	bar.window.Map()
}

func (bar *Bar) drawWorkspaces() error {
	var err error
	bar.workspaces, err = xwindow.Generate(bar.x)
	if err != nil {
		return err
	}

	err = bar.workspaces.CreateChecked(bar.window.Id, 0, 0, 180, 20, xproto.CwBackPixel, 0x333333)
	if err != nil {
		return err
	}

	var xPos, yPos int
	for i, w := range bar.wm.WorkspaceManager.Workspaces {
		tc := NewColor(230, 230, 230)

		if i == bar.wm.WorkspaceManager.ActiveIndex {
			tc = NewColor(255, 153, 0)
		}

		width := drawWorkspace(bar.x, bar.workspaces, w.Name, xPos, yPos, tc)
		xPos += width
	}

	bar.workspaces.Map()
	return nil
}

func drawWorkspace(x *xgbutil.XUtil, p *xwindow.Window, n string, xPos, yPos int, tc Color) int {
	textImg, err := Text(n, "Hack", tc, 14.0, 60, 30)
	if err != nil {
		log.Printf("[WARN] Can't find font: %v", err)
	}

	img, err := xgraphics.NewBytes(x, textImg)
	if err != nil {
		panic(err)
	}

	win, err := xwindow.Generate(x)
	if err != nil {
		panic(err)
	}

	err = win.CreateChecked(p.Id, 0, 0, 1, 1, xproto.CwBackPixel, 0xffffff)
	if err != nil {
		panic(err)
	}

	win.MoveResize(xPos, 0, 60, 30)

	img.XSurfaceSet(win.Id)
	img.XDraw()
	img.XPaint(win.Id)
	img.Destroy()

	win.Map()
	return 60
}

func runClock(x *xgbutil.XUtil, p *xwindow.Window) {
	ticker := time.NewTicker(1 * time.Second)

	drawClock(x, p)

	go func() {
		for _ = range ticker.C {
			drawClock(x, p)
		}
	}()
}

func drawClock(x *xgbutil.XUtil, p *xwindow.Window) {
	log.Println("Updating clock")

	currentTime := time.Now().Format(time.RFC3339)
	size := 18.0

	fd, err := ioutil.ReadFile("/usr/share/fonts/TTF/Hack-Regular.ttf")
	if err != nil {
		panic(err)
	}

	font, err := truetype.Parse(fd)
	if err != nil {
		panic(err)
	}

	w, _ := Extents(currentTime, font, size)
	wg, err := p.Geometry()
	if err != nil {
		panic(err)
	}

	tc := NewColor(230, 230, 230)
	textImg, err := Text(currentTime, "Hack", tc, 14.0, 150, 30)
	if err != nil {
		panic(err)
	}

	img, err := xgraphics.NewBytes(x, textImg)
	if err != nil {
		panic(err)
	}

	win, err := xwindow.Generate(x)
	if err != nil {
		panic(err)
	}

	err = win.CreateChecked(p.Id, 0, 0, 1, 1, xproto.CwBackPixel, 0xffffff)
	if err != nil {
		panic(err)
	}

	win.MoveResize(wg.Width()-w-3, 0, 60, 30)

	img.XSurfaceSet(win.Id)
	img.XDraw()
	img.XPaint(win.Id)
	img.Destroy()

	win.Map()
}

type Color struct {
	R uint32
	G uint32
	B uint32
	A uint32
}

func NewColor(r, g, b uint32) Color {
	return Color{r, g, b, 255}
}

func (c Color) ToFloat64s() (r, g, b float64) {
	r = float64(c.R) / 255.0
	g = float64(c.G) / 255.0
	b = float64(c.B) / 255.0

	return
}

func (c Color) RGBA() (r, g, b, a uint32) {
	return c.R, c.G, c.B, c.A
}

func Text(text, font string, color Color, size float64, width, height int) ([]byte, error) {
	surf := cairo.ImageSurfaceCreate(cairo.FormatARGB32, width, height)

	cr := cairo.Create(surf.Surface)

	fr, fg, fb := color.ToFloat64s()
	cr.SetSourceRGB(0.2, 0.2, 0.2)
	cr.PaintWithAlpha(1.0)
	cr.SetAntialias(cairo.AntialiasBest)
	cr.SetSourceRGB(fr, fg, fb)
	cr.SelectFontFace(font, cairo.FontSlantNormal, cairo.FontWeightNormal)
	cr.SetFontSize(size)
	cr.MoveTo(float64(width)/10, float64(height)/2)
	cr.ShowText(text)

	buf := bytes.NewBuffer(nil)
	if err := surf.WriteToPNG(buf); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

func Extents(text string, font *truetype.Font, size float64) (int, int) {
	return xgraphics.Extents(font, size, text)
}
