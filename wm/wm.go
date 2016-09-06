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
)

// WM represents the window manager entity
type WM struct {
	Clients          []Client
	FocusedClient    Client
	CurrentWorkspace *Workspace
	Workspaces       []*Workspace
}

// NewWindowManager creates a new window manager
func NewWindowManager() (*WM, error) {
	log.Printf("[DEBUG] Create the window manager")
	wm := &WM{
		Clients:    make([]Client, 0),
		Workspaces: make([]*Workspace, 0),
	}

	return wm, nil
}

// Destroy destroy the window manager
func (wm *WM) Destroy() {
	log.Printf("[DEBUG] Destroy the window manager")
}

// Add add a workspace to the window manager
func (wm *WM) Add(ws *Workspace) error {
	if wm.HasWorkspace(ws.ID()) {
		return fmt.Errorf("Workspace already added to the window manager.")
	}
	wm.Workspaces = append(wm.Workspaces, ws)
	if wm.CurrentWorkspace == nil {
		wm.CurrentWorkspace = ws
		wm.CurrentWorkspace.Show()
	}
	return nil
}

// HasWorkspace check if the window manager have this workspace or not
func (wm *WM) HasWorkspace(id string) bool {
	for _, ws := range wm.Workspaces {
		if ws.ID() == id {
			return true
		}
	}
	return false
}
