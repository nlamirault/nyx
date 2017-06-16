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
	"fmt"
	"io/ioutil"
	"os"
	"testing"
)

func TestGetConfiguration(t *testing.T) {
	templateFile, err := ioutil.TempFile("", "configuration")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(templateFile.Name())
	data := []byte(`# Test configuration file

[keybindings]
closeWindow = "Mod4-w"
execLauncher = "Mod4-r"
nextWorkspace = "Mod4-n"
previousWorkspace = "Mod4-p"

[workspaces]
[workspaces.main]
icon = ""
[workspaces.web]
icon = ""
[workspaces.media]
icon = ""

`)
	err = ioutil.WriteFile(templateFile.Name(), data, 0700)
	if err != nil {
		t.Fatal(err)
	}
	configuration, err := LoadFileConfig(templateFile.Name())
	if err != nil {
		t.Fatalf("Error with configuration: %v", err)
	}
	fmt.Printf("Configuration : %#v\n", configuration)

	if configuration.Keybindings.CloseWindow != "Mod4-w" ||
		configuration.Keybindings.ExecLauncher != "Mod4-r" ||
		configuration.Keybindings.NextWorkspace != "Mod4-n" ||
		configuration.Keybindings.PreviousWorkspace != "Mod4-p" {
		t.Fatalf("Invalid keybindings: %s", configuration.Keybindings)
	}
	if len(configuration.Workspaces) != 3 {
		t.Fatalf("Invalid workspaces: %s", configuration.Workspaces)
	}
}
