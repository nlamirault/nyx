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
	"log"

	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/keybind"
	"github.com/BurntSushi/xgbutil/xevent"
	"github.com/BurntSushi/xgbutil/xwindow"
	// "github.com/nlamirault/nyx/keybinds"
)

type WindowManager struct {
	X                *xgbutil.XUtil
	WorkspaceManager *WorkspaceManager
	// Root *xwindow.Window
}

func New(keybindings map[string]string) (*WindowManager, error) {
	x, err := xgbutil.NewConn()
	if err != nil {
		return nil, err
	}

	root := xwindow.New(x, x.RootWin())
	evMasks := xproto.EventMaskPropertyChange |
		xproto.EventMaskFocusChange |
		xproto.EventMaskButtonPress |
		xproto.EventMaskButtonRelease |
		xproto.EventMaskStructureNotify |
		xproto.EventMaskSubstructureRedirect |
		xproto.EventMaskSubstructureRedirect |
		xproto.EventMaskKeyPress |
		xproto.EventMaskKeyRelease
	if err := root.Listen(evMasks); err != nil {
		log.Printf("[ERROR] Root listen events: %v", err)
		return nil, err
	}

	wm := &WindowManager{
		X:                x,
		WorkspaceManager: NewWorkspaceManager(0),
	}
	log.Printf("[DEBUG] Window Manager using %d workspaces", len(wm.WorkspaceManager.Workspaces))
	wm.WorkspaceManager.Add(x, "0")
	wm.WorkspaceManager.Activate(0)

	// bar, err := NewBar(x, wm)
	// if err != nil {
	// 	log.Printf("[ERROR] Can't create bar: %v", err)
	// 	return nil, err
	// }
	// bar.Draw()

	// keybinds.New(wm.X, keybindings)

	keybind.Initialize(x)

	xevent.MapRequestFun(wm.mapRequestHandler).Connect(x, x.RootWin())
	xevent.ConfigureRequestFun(wm.configureRequestHandler).Connect(x, x.RootWin())

	// wm.setupEvents()

	// err = keybind.KeyPressFun(wm.onActivateNextWorkspace).Connect(x, x.RootWin(),
	// 	conf.KeyBindingNextWorkspace, true)
	// if err != nil {
	// 	panic(err)
	// }

	// err = keybind.KeyPressFun(wm.onActivatePreviousWorkspace).Connect(x, x.RootWin(),
	// 	conf.KeyBindingPreviousWorkspace, true)
	// if err != nil {
	// 	panic(err)
	// }

	err = keybind.KeyPressFun(wm.exitKeyPressHandler).Connect(x, x.RootWin(), "Mod4-e", true)

	return wm, nil
}

func (wm *WindowManager) Run() {
	xevent.Main(wm.X)
}

func (wm *WindowManager) Destroy() {
	wm.X.Conn().Close()
}

func (wm *WindowManager) activeWorkspace() *Workspace {
	return wm.WorkspaceManager.Workspaces[wm.WorkspaceManager.ActiveIndex]
}

func (wm *WindowManager) activateNextWorkspace() {
	wm.WorkspaceManager.Activate(wm.WorkspaceManager.NextIndex())
}

func (wm *WindowManager) activatePreviousWorkspace() {
	wm.WorkspaceManager.Activate(wm.WorkspaceManager.PreviousIndex())
}

func (wm *WindowManager) onActivateNextWorkspace(x *xgbutil.XUtil, e xevent.KeyPressEvent) {
	wm.activateNextWorkspace()
}

func (wm *WindowManager) onActivatePreviousWorkspace(x *xgbutil.XUtil, e xevent.KeyPressEvent) {
	wm.activatePreviousWorkspace()
}

func (wm *WindowManager) mapRequestHandler(x *xgbutil.XUtil, e xevent.MapRequestEvent) {
	log.Printf("[DEBUG] Event map request")
	x.Grab()
	defer x.Ungrab()
	ws := wm.activeWorkspace()
	if ws == nil {
		log.Printf("[WARN] No workspace available.")
		return
	}
	win, err := NewWindow(x, ws.WindowID(), e.Window)
	if err != nil {
		log.Printf("[ERROR] Can't create window for map request: %v", err)
		return
	}
	win.Draw()
}

func (wm *WindowManager) configureRequestHandler(x *xgbutil.XUtil, ev xevent.ConfigureRequestEvent) {
	log.Printf("[DEBUG] Event configure request")
}

func (wm *WindowManager) exitKeyPressHandler(x *xgbutil.XUtil, e xevent.KeyPressEvent) {
	log.Printf("[DEBUG] Event keypress exit")
	xevent.Quit(x)
}
