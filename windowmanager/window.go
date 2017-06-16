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
	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/ewmh"
	"github.com/BurntSushi/xgbutil/xevent"
	"github.com/BurntSushi/xgbutil/xwindow"
)

type Window struct {
	X           *xgbutil.XUtil
	child       *xwindow.Window
	parent      *xwindow.Window
	workspaceId xproto.Window
}

func newWindow(x *xgbutil.XUtil, workspaceId xproto.Window, id xproto.Window) (*Window, error) {
	child := xwindow.New(x, id)
	pw, err := xwindow.Generate(x)
	if err != nil {
		return nil, err
	}

	win := Window{
		X:           x,
		child:       child,
		parent:      pw,
		workspaceId: workspaceId,
	}

	win.bindEvents()
	win.reparentChild()
	win.Activate()
	return &win, nil
}

func (w *Window) reparentChild() error {
	cg, err := w.child.Geometry()
	if err != nil {
		return err
	}

	if err := w.parent.CreateChecked(w.workspaceId, cg.X(), cg.Y(), cg.Width()*2, cg.Height()*2, xproto.CwBackPixel, 0x111111); err != nil {
		return err
	}

	if err := xproto.ReparentWindowChecked(w.X.Conn(), w.child.Id, w.parent.Id, 0, 0).Check(); err != nil {
		return err
	}

	return nil
}

func (w *Window) bindEvents() {
	evMask := xproto.EventMaskStructureNotify
	if err := w.child.Listen(evMask); err != nil {
		panic(err)
	}
	xevent.DestroyNotifyFun(func(x *xgbutil.XUtil, e xevent.DestroyNotifyEvent) {
		w.Destroy()
	}).Connect(w.X, w.child.Id)
}

func (w *Window) Maximize() error {
	rg := xwindow.RootGeometry(w.X)
	w.parent.MoveResize(rg.X(), rg.Y(), rg.Width(), rg.Height())
	return nil
}

func (w *Window) Destroy() {
	w.parent.Destroy()
}

func (w *Window) Draw() {
	w.parent.Map()
	w.child.Map()
}

func (w *Window) Activate() error {
	return ewmh.ActiveWindowSet(w.X, w.child.Id)
}
