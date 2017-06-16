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
	"github.com/BurntSushi/xgbutil/keybind"
	"github.com/BurntSushi/xgbutil/xevent"
	"github.com/BurntSushi/xgbutil/xwindow"
	"github.com/golang/glog"

	"github.com/nlamirault/nyx/config"
)

type WindowManager struct {
	X                *xgbutil.XUtil
	Root             *xwindow.Window
	conf             *config.Configuration
	WorkspaceManager *WorkspaceManager
}

func New(conf *config.Configuration) (*WindowManager, error) {
	glog.V(2).Infof("Create the window manager")

	x, err := xgbutil.NewConnDisplay(":1")
	if err != nil {
		return nil, err
	}

	wm := &WindowManager{
		X:                x,
		WorkspaceManager: newWorkspaceManager(len(conf.Workspaces)),
	}
	wm.WorkspaceManager.Add(x, conf)
	wm.WorkspaceManager.Activate(0)

	panel, err := newPanel(x, wm)
	if err != nil {
		return nil, err
	}
	panel.Run()

	root := xwindow.New(x, x.RootWin())
	evMasks := xproto.EventMaskPropertyChange |
		xproto.EventMaskFocusChange |
		xproto.EventMaskButtonPress |
		xproto.EventMaskButtonRelease |
		xproto.EventMaskStructureNotify |
		xproto.EventMaskSubstructureRedirect |
		xproto.EventMaskSubstructureRedirect

	if err := root.Listen(evMasks); err != nil {
		panic(err)
	}

	keybind.Initialize(x)

	xevent.MapRequestFun(wm.onMapRequest).Connect(x, x.RootWin())
	xevent.ConfigureRequestFun(wm.onConfigureRequest).Connect(x, x.RootWin())

	if err := keybind.KeyPressFun(wm.onActivateNextWorkspace).Connect(x, x.RootWin(), conf.Keybindings.NextWorkspace, true); err != nil {
		return nil, err
	}

	if err := keybind.KeyPressFun(wm.onActivatePreviousWorkspace).Connect(x, x.RootWin(), conf.Keybindings.PreviousWorkspace, true); err != nil {
		return nil, err
	}

	// if err := keybind.KeyPressFun(onExecLauncher).Connect(x, x.RootWin(), conf.Keybindings.ExecLauncher, true); err != nil {
	// 	return nil, err
	// }

	return wm, nil
}

func (wm *WindowManager) activeWorkspace() *Workspace {
	return wm.WorkspaceManager.Workspaces[wm.WorkspaceManager.ActiveIndex()]
}

// func (wm *WindowManager) activateNextWorkspace() {
// 	glog.V(2).Info("Event: next workspace")
// 	wm.WorkspaceManager.Activate(wm.WorkspaceManager.NextIndex())
// }

// func (wm *WindowManager) activatePreviousWorkspace() {
// 	glog.V(2).Info("Event: previous workspace")
// 	wm.WorkspaceManager.Activate(wm.WorkspaceManager.PreviousIndex())
// }

func (wm *WindowManager) onActivateNextWorkspace(x *xgbutil.XUtil, e xevent.KeyPressEvent) {
	glog.V(2).Infof("Event: next workspace: %s", e)
	wm.WorkspaceManager.Activate(wm.WorkspaceManager.NextIndex())
}

func (wm *WindowManager) onActivatePreviousWorkspace(x *xgbutil.XUtil, e xevent.KeyPressEvent) {
	glog.V(2).Infof("Event: previous workspace: %s", e)
	wm.WorkspaceManager.Activate(wm.WorkspaceManager.PreviousIndex())
}

func (wm *WindowManager) onMapRequest(x *xgbutil.XUtil, e xevent.MapRequestEvent) {
	glog.V(2).Infof("Event: map request: %s", e)
	x.Grab()
	defer x.Ungrab()

	w := newWindow(x, wm.activeWorkspace().WindowId(), e.Window)
	w.Draw()
}

func (wm *WindowManager) onConfigureRequest(x *xgbutil.XUtil, e xevent.ConfigureRequestEvent) {
	glog.V(2).Infof("Event: configure request: %s", e)
	xwindow.New(x, e.Window).Configure(int(e.ValueMask), int(e.X), int(e.Y),
		int(e.Width), int(e.Height), e.Sibling, e.StackMode)
}

func (wm *WindowManager) Run() {
	xevent.Main(wm.X)
}
