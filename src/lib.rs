#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

//! A cron expression parser and schedule explorer
//! # Example
//! ```
//! extern crate chrono;
//! extern crate cron_schedule;
//!
//! use cron_schedule::Schedule;
//! use chrono::Utc;
//! use std::str::FromStr;
//!
//! fn main() {
//!   //               sec  min   hour   day of month   month   day of week   year
//!   let expression = "0   30   9,12,15     1,15       May-Aug  Mon,Wed,Fri  2018/2";
//!   let schedule = Schedule::from_str(expression).unwrap();
//!   println!("Upcoming fire times:");
//!   for datetime in schedule.upcoming().take(10) {
//!     println!("-> {}", datetime);
//!   }
//! }
//!
//! /*
//! Upcoming fire times:
//! -> 2018-06-01 09:30:00 UTC
//! -> 2018-06-01 12:30:00 UTC
//! -> 2018-06-01 15:30:00 UTC
//! -> 2018-06-15 09:30:00 UTC
//! -> 2018-06-15 12:30:00 UTC
//! -> 2018-06-15 15:30:00 UTC
//! -> 2018-08-01 09:30:00 UTC
//! -> 2018-08-01 12:30:00 UTC
//! -> 2018-08-01 15:30:00 UTC
//! -> 2018-08-15 09:30:00 UTC
//! */
//! ```
pub mod error;
mod ordinal;
mod parsing;
mod queries;
mod schedule;
mod specifier;
mod time_unit;

pub use crate::schedule::Schedule;
pub use crate::time_unit::TimeUnitSpec;
