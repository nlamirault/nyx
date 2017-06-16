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
	"github.com/BurntSushi/xgbutil"

	"github.com/nlamirault/nyx/config"
)

type WorkspaceManager struct {
	Workspaces  []*Workspace
	activeIndex int
}

func newWorkspaceManager(initialCount int) *WorkspaceManager {
	return &WorkspaceManager{
		Workspaces: make([]*Workspace, 0, initialCount),
	}
}
func (wm *WorkspaceManager) Add(x *xgbutil.XUtil, conf *config.Configuration) error {
	for _, wsConf := range conf.Workspaces {
		workspace, err := newWorkspace(x, &wsConf)
		if err != nil {
			return err
		}
		wm.Workspaces = append(wm.Workspaces, workspace)
	}
	return nil
}

func (wm *WorkspaceManager) Names() []string {
	names := make([]string, 0, len(wm.Workspaces))
	for _, ws := range wm.Workspaces {
		names = append(names, ws.Name())
	}

	return names
}

func (wm *WorkspaceManager) NextIndex() int {
	var index int
	if wm.activeIndex != len(wm.Workspaces)-1 {
		index = wm.activeIndex + 1
	}

	return index
}

func (wm *WorkspaceManager) PreviousIndex() int {
	index := wm.activeIndex - 1

	if index == -1 {
		index = len(wm.Workspaces) - 1
	}

	return index
}

func (wm *WorkspaceManager) Activate(index int) {
	wm.Workspaces[wm.activeIndex].Deactivate()
	wm.Workspaces[index].Activate()
	wm.activeIndex = index
	// broadcaster.Trigger(broadcaster.EventWorkspaceChanged)
}

func (wm *WorkspaceManager) ActiveIndex() int {
	return wm.activeIndex
}
