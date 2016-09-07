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

package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"path"
	"runtime"
	"strings"

	"github.com/nlamirault/nyx/logging"
	"github.com/nlamirault/nyx/version"
	"github.com/nlamirault/nyx/wm"
)

const (
	application = "Nyx"
)

var (
	flagDebug   bool
	flagVersion bool
	flagReplace bool
)

func init() {
	// parse flags
	flag.BoolVar(&flagDebug, "d", false, "run in debug mode")
	flag.BoolVar(&flagVersion, "v", false, "show version")
	flag.BoolVar(&flagReplace, "replace", false, "If another window manager is running, replace it.")
	flag.Usage = usage
	flag.Parse()
}

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: %s [flags]\n", path.Base(os.Args[0]))
	flag.VisitAll(func(fg *flag.Flag) {
		fmt.Printf("--%s=\"%s\"\n\t%s\n", fg.Name, fg.DefValue,
			strings.Replace(fg.Usage, "\n", "\n\t", -1))
	})
	os.Exit(1)
}

func getTitle() string {
	return fmt.Sprintf("%s - v%s", application, version.Version)
}

func setupWindowManager(flagDebug bool) {
	defaultKeybindings := make(map[string]string)
	defaultKeybindings["Mod4-return"] = "exec termite"
	defaultKeybindings["Mod4-j"] = "exec terminator"
	defaultKeybindings["Mod4-Escape"] = "exit"

	xwm, err := wm.New(defaultKeybindings)
	if err != nil {
		log.Fatalf("[ERROR] Can't create window manager : %v", err)
	}
	defer xwm.Destroy()

	// keybinds.New(xgb.X, defaultKeybindings)

	// if flagDebug {
	// 	xgb.Debug()
	// }

	xwm.Run()
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	if flagVersion {
		fmt.Printf("%s v%s\n", application, version.Version)
		os.Exit(0)
	}
	if flagDebug {
		logging.SetLogging("DEBUG")
	} else {
		logging.SetLogging("INFO")
	}

	setupWindowManager(flagDebug)
}
