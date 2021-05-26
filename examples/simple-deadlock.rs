use std::error::Error;
use std::sync::Arc;
use tokio::sync::oneshot;
use tokio::sync::Notify;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // This is a channel, for sender to signal reciever that it sees flag to be true.
    let (tx, mut rx) = oneshot::channel::<i64>();

    let notify_send = Arc::new(Notify::new());
    let notify_recv = Arc::clone(&notify_send);

    let sender = tokio::spawn(async move {
        println!("Waiting to be notified ...");
        notify_recv.notified().await;
        println!("Sending value over channel");
        let _ = tx.send(100);
    });

    let reciever = tokio::spawn(async move {
        println!("Trying to recieve value ...");
        if let Ok(data) = rx.try_recv() {
            println!("Recieved {} over channel", data);
            println!("Notifying other ...");
            notify_send.notify_one();
        }
    });

    // I am pretty sure you can see why a deadlock occurs.

    sender.await?;
    reciever.await?;
    Ok(())
}
