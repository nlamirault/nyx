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
)

// Workspace defines workspace for the window manager
type Workspace struct {
	id      string
	clients []Client
	visible bool
}

// NewWorkspace creates a new workspace
func NewWorkspace(id string) *Workspace {
	return &Workspace{id: id}
}

// ID returns the id of this workspace.
func (ws *Workspace) ID() string {
	return ws.id
}

// Add add a client to this workspace.
func (ws *Workspace) Add(client Client) error {
	if ws.HasClient(client.Id()) {
		return fmt.Errorf("Client already added into this workspace")
	}
	ws.clients = append(ws.clients, client)
	if ws.Visible() {
		client.Show()
	}
	return nil
}

// Remove removes a client from this workspace.
func (ws *Workspace) Remove(client Client) error {
	idx, err := ws.findClient(client.Id())
	if err != nil {
		return err
	}
	ws.clients[idx] = ws.clients[len(ws.clients)-1]
	ws.clients = ws.clients[:len(ws.clients)-1]
	return nil
}

func (ws *Workspace) HasClient(id uint) bool {
	_, err := ws.findClient(id)
	return err == nil
}

func (ws *Workspace) findClient(id uint) (int, error) {
	for idx, c := range ws.clients {
		if c.Id() == id {
			return idx, nil
		}
	}
	return -1, fmt.Errorf("Client not found")
}

// Visible returns true if this workspace is currently visible.
func (ws *Workspace) Visible() bool {
	return ws.visible
}

// Show shows this workspace and all clients in it.
func (ws *Workspace) Show() {
	ws.visible = true
	for _, c := range ws.clients {
		c.Show()
	}
}

// Hide hides this workspace and all clients in it.
func (ws *Workspace) Hide() {
	ws.visible = false
	for _, c := range ws.clients {
		c.Hide()
	}
}
