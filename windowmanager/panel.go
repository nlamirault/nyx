// Copyright (C) 2016, 2017  Nicolas Lamirault <nicolas.lamirault@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package windowmanager

import (
	"io/ioutil"
	"time"

	"github.com/BurntSushi/freetype-go/freetype/truetype"
	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/xgraphics"
	"github.com/BurntSushi/xgbutil/xwindow"
	"github.com/golang/glog"

	"github.com/nlamirault/nyx/config"
	"github.com/nlamirault/nyx/render"
)

type Panel struct {
	x          *xgbutil.XUtil
	window     *xwindow.Window
	conf       *config.Configuration
	wm         *WindowManager
	workspaces *xwindow.Window
}

func newPanel(x *xgbutil.XUtil, wm *WindowManager) (*Panel, error) {
	glog.V(2).Info("Creating new panel")

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

	p := &Panel{x: x, window: win, wm: wm}
	p.drawWorkspaces()

	// broadcaster.Listen(broadcaster.EventWorkspaceChanged, func() {
	// 	p.drawWorkspaces()
	// })

	return p, nil
}

func (p *Panel) Run() {
	p.window.Map()
}
func (p *Panel) drawWorkspaces() error {
	oldWorkspaces := p.workspaces
	newWorkspaces, err := xwindow.Generate(p.x)
	if err != nil {
		return err
	}
	p.workspaces = newWorkspaces

	if err := p.workspaces.CreateChecked(p.window.Id, 0, 0, 180, 20, xproto.CwBackPixel, 0x333333); err != nil {
		return err
	}

	var xPos, yPos int
	for i, w := range p.wm.WorkspaceManager.Workspaces {
		tc := render.NewColor(230, 230, 230)

		if i == p.wm.WorkspaceManager.ActiveIndex() {
			tc = render.NewColor(255, 153, 0)
		}

		width, err := drawWorkspace(p.x, p.workspaces, w.Name(), xPos, yPos, tc)
		if err != nil {
			return err
		}
		xPos += width
	}

	p.workspaces.Map()
	if oldWorkspaces != nil {
		oldWorkspaces.Destroy()
	}
	return nil
}

func drawWorkspace(x *xgbutil.XUtil, p *xwindow.Window, n string, xPos, yPos int, tc render.Color) (int, error) {
	textImg, err := render.Text(n, "SourceCodePro", tc, 14.0, 60, 30)
	if err != nil {
		return -1, err
	}

	img, err := xgraphics.NewBytes(x, textImg)
	if err != nil {
		return -1, err
	}

	win, err := xwindow.Generate(x)
	if err != nil {
		return -1, err
	}

	err = win.CreateChecked(p.Id, 0, 0, 1, 1, xproto.CwBackPixel, 0xffffff)
	if err != nil {
		return -1, err
	}

	win.MoveResize(xPos, 0, 60, 30)

	img.XSurfaceSet(win.Id)
	img.XDraw()
	img.XPaint(win.Id)
	img.Destroy()

	win.Map()
	return 60, nil
}

func runClock(x *xgbutil.XUtil, p *xwindow.Window) {
	ticker := time.NewTicker(1 * time.Second)
	// drawClock(x, p)
	go func() {
		for _ = range ticker.C {
			if err := drawClock(x, p); err != nil {
				glog.Infof("Can't render clock: %s", err)
			}
		}
	}()
}

func drawClock(x *xgbutil.XUtil, p *xwindow.Window) error {
	glog.V(4).Info("Updating clock")

	currentTime := time.Now().Format("15:04:05")
	size := 18.0

	fd, err := ioutil.ReadFile("/usr/share/fonts/TTF/DejaVuSans.ttf")
	if err != nil {
		return err
	}

	font, err := truetype.Parse(fd)
	if err != nil {
		return err
	}

	w, _ := render.Extents(currentTime, font, size)
	wg, err := p.Geometry()
	if err != nil {
		return err
	}

	tc := render.NewColor(230, 230, 230)
	glog.V(4).Infof("Time: %s", currentTime)
	textImg, err := render.Text(currentTime, "SourceCodePro", tc, 14.0, 70, 30)
	if err != nil {
		return err
	}

	img, err := xgraphics.NewBytes(x, textImg)
	if err != nil {
		return err
	}

	win, err := xwindow.Generate(x)
	if err != nil {
		return err
	}

	err = win.CreateChecked(p.Id, 0, 0, 1, 1, xproto.CwBackPixel, 0xffffff)
	if err != nil {
		return err
	}

	win.MoveResize(wg.Width()-w-3, 0, 70, 30)
	img.XSurfaceSet(win.Id)
	img.XDraw()
	img.XPaint(win.Id)
	img.Destroy()
	win.Map()
	return nil
}
