package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"github.com/hyperledger/fabric/core/chaincode/shim"
	pb "github.com/hyperledger/fabric/protos/peer"
	"strings"
)

type MessageStore struct{}

type Message struct {
	ID    string `json:"ID"`
	Value string `json:"value"`
}

// Main function starts up the chaincode in the container during instantiate
//
func main() {
	if err := shim.Start(new(MessageStore)); err != nil {
		fmt.Printf("Main: Error starting MessageStore chaincode: %s", err)
	}
}

// Init is called during Instantiate transaction after the chaincode container
// has been established for the first time, allowing the chaincode to
// initialize its internal data. Note that chaincode upgrade also calls this
// function to reset or to migrate data, so be careful to avoid a scenario
// where you inadvertently clobber your ledger's data!
//
func (cc *MessageStore) Init(stub shim.ChaincodeStubInterface) pb.Response {
	// Validate supplied init parameters, in this case zero arguments!

	// TODO implement the check for the init parameters
	//function, args := stub.GetFunctionAndParameters()

	// TODO add a return 'success' statement
	return shim.Success(nil)

	//return pb.Response{}
}


func (cc *MessageStore) Invoke(stub shim.ChaincodeStubInterface) pb.Response {

	function, args := stub.GetFunctionAndParameters()

	function = strings.ToLower(function)

	switch function {

	case "write":
		return cc.write(stub, args)

	case "read":
		return cc.read(stub, args)
	default:
		msg := "Invalid method! valid methods are 'write' or 'read'!"
		return shim.Error(msg)
	}
}

// Write an ID and string to the blockchain
//
func (cc *MessageStore) write(stub shim.ChaincodeStubInterface, args []string) pb.Response {

	// extract the ID and value from the arguments the content and object id
	// TODO make sure that just two values have been passed as arguments
	message, err := getMessageFromArgs(args)
	if err != nil {
		return shim.Error("Wrong number of arguments; Expecting an ID and value")

	}
	// key should be lowercase
	id := strings.ToLower(message.ID)

	// marshal message struct to json
	// can be returned directly
	// allows more sophisticated search using couchdb /mq
	// TODO add variable for json return value
	json, err := json.Marshal(message)

	// Validate that this ID does not yet exist
	// TODO check if the key has already been assigned
	record, err := stub.GetState(id)
	if err != nil {
		return shim.Error(err.Error())
	}

	if record != nil {
		return shim.Error("The record is existed")
	}
	// Write the message
	// TODO if key has not been assigned yet write it to the ledger
	err = stub.PutState(id, json)

	// TODO if successful return 'shim.Success(nil)'
	//return pb.Response{}
	if err != nil {
		return shim.Error(err.Error())
	}

	return shim.Success(nil)
}

// Read a string from the blockchain, given its ID
//
// TODO implement the header of the 'read' function
func (cc *MessageStore) read(stub shim.ChaincodeStubInterface, args []string) pb.Response {
	if len(args) != 1 {
		return shim.Error("Wrong number of arguments;")
	}

	id := strings.ToLower(args[0])

	if record, err := stub.GetState(id); err != nil || record == nil {
		return shim.Error("Read: invalid ID")
	} else {
		return shim.Success(record)
	}
}

func getMessageFromArgs(args []string) (message Message, err error) {
	if strings.Contains(args[0], "\"ID\"") == false ||
		strings.Contains(args[0], "\"Value\"") == false {
		return message, errors.New("Unknown field: Input JSON does not comply to schema")
	}

	err = json.Unmarshal([]byte(args[0]), &message)

	if err != nil {
		return message, err
	}

	return message, nil
}
