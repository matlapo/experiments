use std::io::SeekFrom;
use uuid::Uuid;
use anyhow::Result;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, BufWriter};
use std::str;
use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};
use std::collections::BTreeMap;
use std::env;
use std::time::Instant;

pub fn read_uuids() -> Result<Vec<Uuid>> {
    let mut uuids: Vec<Uuid> = vec![];
    let args: Vec<String> = env::args().collect();
    for arg in &args[1..] {
        let uuid = Uuid::parse_str(&arg)?;
        uuids.push(uuid);
    }
    return Ok(uuids);
}

fn create_db() -> Result<()> {
    let events = File::open("events.json")?;

    let db = File::create("db.bin")?;

    let mut f_in = BufReader::new(events);
    let mut f_out = BufWriter::new(db);

    let mut buf = vec![];
    let mut bmap = BTreeMap::new();

    loop {
        let n = f_in.read_until(b'\n', &mut buf)?;
        if n > 0 {
            let key_str = str::from_utf8(&buf[9..45])?; // yolo
            let key = Uuid::parse_str(&key_str)?;

            let count = bmap.entry(key).or_insert(0);
            *count += 1;

            buf.clear();
        } else {
            break;
        }
    }

    println!("LEN {:?}", bmap.len());

    for (key, count) in bmap.iter() {
        let num = key.to_u128_le();
        f_out.write_u128::<LittleEndian>(num)?;
        f_out.write_u64::<LittleEndian>(*count)?;
    }

    f_out.flush()?;

    return Ok(());
}

// The number of unique uuids is 100x less than the total
// number of uuid in the original set. Thus loading all of
// them might make sense, especially since only the first
// query will bear a heavy cost.
fn query_load_all() -> Result<()> {
    let uuids = read_uuids()?;

    let now = Instant::now();
    let f = File::open("db.bin")?;
    let mut db = BufReader::new(f);

    let mut bmap = BTreeMap::new();
    let mut buf: Vec<u8> = vec![];
    let _ = db.read_to_end(&mut buf)?;

    for i in (0..buf.len()).step_by(24) {
        let key = LittleEndian::read_u128(&buf[i..i+16]);
        let count = LittleEndian::read_u64(&buf[i+16..i+24]);
        bmap.insert(key, count);
    }

    for uuid in uuids {
        if let Some(count) = bmap.get(&uuid.to_u128_le()) {
            println!("BMAP {:?}: {:?}", uuid, count);
        }
    }

    // println!("{}µs", now.elapsed().as_micros());

    return Ok(());
}

fn query_db() -> Result<()> {
    let uuids = read_uuids()?;
    let target = uuids[0].to_u128_le();

    let now = Instant::now();
    let f = File::open("db.bin")?;
    let mut db = BufReader::new(f);
    let mut buf = vec![0u8; 24];

    let count = 1000;
    let mut lower: u64 = 0;
    let mut upper: u64 = count * 24;
    let mut mid = (upper - lower) / 2;
    let mut mid_aligned = ((mid / 24) as u64) * 24;
    let mut key;

    while upper - lower >= 24 {
        db.seek(SeekFrom::Start(mid_aligned))?;
        db.read_exact(&mut buf)?;
        key = LittleEndian::read_u128(&buf[..16]);

        if target == key {
            let count = LittleEndian::read_u64(&buf[16..24]);
            println!("BIN SEARCH {:?}: {:?}", Uuid::from_u128_le(key), count);
            return Ok(());
        }
        else if target < key {
            println!("====== left pane ======");
            println!("before: low {:?} mid {:?} high {:?}", lower, mid_aligned, upper);
            upper = mid_aligned;
            mid = (upper + lower) / 2;
            mid_aligned = ((mid / 24) as u64) * 24;
            println!("after: low {:?} mid {:?} high {:?}", lower, mid_aligned, upper);
            let count = LittleEndian::read_u64(&buf[16..24]);
            println!("{:?}: {:?}", Uuid::from_u128_le(key), count);
        }
        else {
            println!("====== right pane ======");
            println!("before low {:?} mid {:?} high {:?}", lower, mid_aligned, upper);
            lower = mid_aligned;
            mid = (upper + lower) / 2;
            mid_aligned = ((mid / 24) as u64) * 24;
            println!("after: low {:?} mid {:?} high {:?}", lower, mid_aligned, upper);
            let count = LittleEndian::read_u64(&buf[16..24]);
            println!("{:?}: {:?}", Uuid::from_u128_le(key), count);
        }
    }

    return Ok(());
}

fn main() -> Result<()> {
    create_db()?;
    // query_db()?;
    query_load_all()?;
    return Ok(());
}

