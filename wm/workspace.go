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
	"fmt"
	"log"

	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/xwindow"
)

type Workspace struct {
	Name   string
	window *xwindow.Window
}

func NewWorkspace(x *xgbutil.XUtil, name string) (*Workspace, error) {
	x.Grab()
	defer x.Ungrab()

	rg := xwindow.RootGeometry(x)
	ws, err := xwindow.Generate(x)
	if err != nil {
		return nil, fmt.Errorf("Failed to generate workspace: %v", err)
	}

	err = ws.CreateChecked(x.RootWin(), 0, 20, rg.Width(), rg.Height()-20,
		xproto.CwBackPixel, 0x666666)
	if err != nil {
		return nil, fmt.Errorf("Failed to create workspace: %v", err)
	}

	err = xproto.ReparentWindowChecked(x.Conn(), ws.Id, x.RootWin(), 0, 20).Check()
	if err != nil {
		panic(err)
	}

	return &Workspace{
		Name:   name,
		window: ws,
	}, nil
}

func (ws *Workspace) Deactivate() {
	ws.window.Unmap()
}

func (ws *Workspace) Activate() {
	ws.window.Map()
}

func (ws *Workspace) WindowID() xproto.Window {
	return ws.window.Id
}

type WorkspaceManager struct {
	Workspaces  []*Workspace
	ActiveIndex int
}

func NewWorkspaceManager(initialCount int) *WorkspaceManager {
	return &WorkspaceManager{
		Workspaces:  make([]*Workspace, initialCount),
		ActiveIndex: 0,
	}
}

func (wm *WorkspaceManager) Add(x *xgbutil.XUtil, name string) {
	log.Printf("[DEBUG] Add workspace: %s", name)
	ws, err := NewWorkspace(x, name)
	if err != nil {
		log.Printf("[ERROR] Can't add workspace: %v", err)
		return
	}
	wm.Workspaces = append(wm.Workspaces, ws)
	log.Printf("[DEBUG] Workspaces: %d", len(wm.Workspaces))
}

func (wm *WorkspaceManager) Names() []string {
	names := make([]string, 0, len(wm.Workspaces))
	for _, ws := range wm.Workspaces {
		names = append(names, ws.Name)
	}

	return names
}

func (wm *WorkspaceManager) NextIndex() int {
	var index int
	if wm.ActiveIndex != len(wm.Workspaces)-1 {
		index = wm.ActiveIndex + 1
	}

	return index
}

func (wm *WorkspaceManager) PreviousIndex() int {
	index := wm.ActiveIndex - 1

	if index == -1 {
		index = len(wm.Workspaces) - 1
	}

	return index
}

func (wm *WorkspaceManager) Activate(index int) {
	log.Printf("[DEBUG] active workspace: %d", index)
	if wm.Workspaces[wm.ActiveIndex] != nil {
		wm.Workspaces[wm.ActiveIndex].Deactivate()
	}
	if wm.Workspaces[index] == nil {
		log.Printf("[WARN] No workspace available for %d", index)
		return
	}
	wm.Workspaces[index].Activate()
	wm.ActiveIndex = index

	// broadcaster.Trigger(broadcaster.EventWorkspaceChanged)
}
