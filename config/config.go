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

package config

import (
	"github.com/BurntSushi/toml"
	"github.com/golang/glog"
)

// Configuration holds configuration for Diablo.
type Configuration struct {
	Keybindings *KeybindingsConfig   `toml:"keybindings"`
	Workspaces  map[string]Workspace `toml:"workspaces"`
}

type Workspace struct {
	Name string
	Icon string
}

type KeybindingsConfig struct {
	CloseWindow       string
	ExecLauncher      string
	NextWorkspace     string
	PreviousWorkspace string
}

// New returns a Configuration with default values
func New() *Configuration {
	return &Configuration{
		Keybindings: &KeybindingsConfig{},
		Workspaces:  map[string]Workspace{},
	}
}

// LoadFileConfig returns a Configuration from reading the specified file (a toml file).
func LoadFileConfig(file string) (*Configuration, error) {
	glog.V(1).Infof("Load configuration from file: %s", file)
	configuration := New()
	if _, err := toml.DecodeFile(file, configuration); err != nil {
		return nil, err
	}
	return configuration, nil
}
