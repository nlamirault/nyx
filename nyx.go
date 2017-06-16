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

package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"

	"github.com/golang/glog"

	"github.com/nlamirault/nyx/config"
	"github.com/nlamirault/nyx/version"
	"github.com/nlamirault/nyx/windowmanager"
)

const (
	app = "nyx"
)

var (
	vrs        bool
	configFile string
)

func init() {
	// parse flags
	flag.BoolVar(&vrs, "version", false, "print version and exit")
	flag.StringVar(&configFile, "config", "", "Configuration file to use.")
	flag.Usage = func() {
		fmt.Fprint(os.Stderr, fmt.Sprintf("%s - v%s\n", app, version.Version))
		flag.PrintDefaults()
	}

	flag.Parse()

	if vrs {
		fmt.Printf("%s\n", version.Version)
		os.Exit(0)
	}
	if len(configFile) == 0 {
		fmt.Printf("Configuration file must not be empty.\n")
		os.Exit(1)
	}

}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	conf, err := config.LoadFileConfig(configFile)
	if err != nil {
		glog.Fatalf("Failed to load configuration: %s", err)
	}

	wm, err := windowmanager.New(conf)
	if err != nil {
		glog.Fatalf("Failed to create the window manager: %s", err)
	}
	glog.V(2).Infof("Starting Nyx...")
	wm.Run()
}
