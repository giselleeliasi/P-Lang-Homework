package main

import (
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

// A little utility that simulates performing a task for a random duration.
func do(seconds int, action ...any) {
	log.Println(action...)
	randomMillis := 500*seconds + rand.Intn(500*seconds)
	time.Sleep(time.Duration(randomMillis) * time.Millisecond)
}

// Order represents a customer's order.
type Order struct {
	id        uint64
	customer  string
	reply     chan *Order
	preparedBy string
}

var (
	nextID atomic.Uint64
	waiter = make(chan *Order, 3) // Waiter can hold up to 3 orders
)

// Cook processes orders from the waiter channel, prepares them, and sends them back through the reply channel.
func Cook(name string) {
	log.Println(name, "starting work")
	for {
		order := <-waiter // Get an order from the waiter
		do(10, name, "is cooking order", order.id, "for", order.customer)
		order.preparedBy = name
		order.reply <- order // Send the prepared order back to the customer
	}
}

// Customer places orders and waits for meals to be prepared.
func Customer(name string, wg *sync.WaitGroup) {
	defer wg.Done() // Decrement the wait group counter when the customer is done

	mealsEaten := 0
	for mealsEaten < 5 {
		order := &Order{
			id:       nextID.Add(1),
			customer: name,
			reply:    make(chan *Order, 1),
		}
		log.Println(name, "placed order", order.id)

		select {
		case waiter <- order: // Try to give the order to the waiter
			meal := <-order.reply // Wait for the meal to be cooked
			do(2, name, "is eating order", meal.id, "prepared by", meal.preparedBy)
			mealsEaten++
		case <-time.After(7 * time.Second): // Timeout if the waiter is too busy
			do(5, name, "is waiting too long, abandoning order", order.id)
		}
	}

	log.Println(name, "is going home")
}

func main() {
	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	customers := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}

	var wg sync.WaitGroup

	// Start the cooks
	go Cook("Remy")
	go Cook("Linguini")
	go Cook("Colette")

	// Start the customers
	for _, customer := range customers {
		wg.Add(1)
		go Customer(customer, &wg)
	}

	// Wait for all customers to finish
	wg.Wait()

	log.Println("Restaurant closing")
}

// package main

// import (
// 	"log"
// 	"math/rand"
// 	"time"
// 	"sync"
// 	"sync/atomic"
// )

// // A little utility that simulates performing a task for a random duration.
// // For example, calling do(10, "Remy", "is cooking") will compute a random
// // number of milliseconds between 5000 and 10000, log "Remy is cooking",
// // and sleep the current goroutine for that much time.

// func do(seconds int, action ...any) {
//     log.Println(action...)
//     randomMillis := 500 * seconds + rand.Intn(500 * seconds)
//     time.Sleep(time.Duration(randomMillis) * time.Millisecond)
// }

// // Implement the rest of the simulation here. You may need to add more imports
// // above.
// 	//make a replay which is a channel that can take the order (hint: you need a pointer)
// 	//Also name of the cook
// 	// a waiter can only hold 3 orders at once
// type Order struct {
// 	id unit64
// 	customer string
// 	var nextId atomic.Unit64
// 	var Waiter = make (chan bool, 3)
// }
// func Cook(name string) {
// 	//log that they are starting
// 	//loop forever
// 	//wait for an ordert form the waiter
// 	//cook it
// 	//put name of the cook i the order
// 	//send it back inot the replay channel: order.replay <- order
// 	select {
// 		case Waiter ....
// 	}
// }
// func Customer(name string, wg *sync.WaitGroup){
// 	for mealsEaten := 0; mealsEaten <5{
// 		//place an order
// 		//select statement so that if the waiter gets it within 7 seconds then you get it from the cook and eat it (mealsEaten ++)
// 		//if you dont get it leave the resturaunt
// 		//do (5, name,"is waiting too long, abandoning order", order.id) 

// 	}
// }

// func main(){
// 	    customers := [10]string{
//         "Ani", "Bai", "Cat", "Dao", "Eve", 
//         "Fay", "Gus", "Hua", "Iza", "Jai",
//     }
// 	//in a loop, start each customer as a gorutine
// 	var wg. Add(1)	
// 	go Customer (customer,&wg)
// 	go cook ("Remy")
// 	go cook ("Linguini")
// 	go cook ("Colette")

//     log.Println("Restaurant closing")
// }