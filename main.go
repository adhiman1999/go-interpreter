package main

import (
	"fmt"
	"os"
	"os/user"

	"github.com/go-interpreter/console"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s!  This is the Cobra programming Language!\n", user.Username)
	fmt.Printf("Feel free to type in commands\n")
	console.Start(os.Stdin, os.Stdout)
}
