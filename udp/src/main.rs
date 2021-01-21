use std::net::UdpSocket;

fn main() -> std::io::Result<()> {
    {
        let mut socket = UdpSocket::bind("127.0.0.1:12345")?;

        const SIZE: usize = 65508;

        let bytes: [u8; SIZE] = [6; SIZE];
        socket.send_to(&bytes, "127.0.0.1:12345")?;

        let mut buf = [0; SIZE];
        let _ = socket.recv_from(&mut buf)?;
        println!("reveived");
    }
    Ok(())
}