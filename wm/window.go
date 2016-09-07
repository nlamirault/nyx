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
	workspaceID xproto.Window
}

func NewWindow(x *xgbutil.XUtil, workspaceID xproto.Window, id xproto.Window) (*Window, error) {
	child := xwindow.New(x, id)
	parent, err := xwindow.Generate(x)
	if err != nil {
		return nil, err
	}

	win := Window{
		X:           x,
		child:       child,
		parent:      parent,
		workspaceID: workspaceID,
	}

	xevent.DestroyNotifyFun(func(x *xgbutil.XUtil, e xevent.DestroyNotifyEvent) {
		win.parent.Destroy()
	}).Connect(win.X, win.child.Id)

	ewmh.ActiveWindowSet(win.X, win.child.Id)
	return &win, nil
}

func (w *Window) Draw() {
	w.parent.Map()
	w.child.Map()
}
