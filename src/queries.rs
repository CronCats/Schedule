use chrono::{DateTime, Datelike, NaiveDateTime, Timelike, Utc};

use crate::ordinal::Ordinal;
use crate::time_unit::{DaysOfMonth, Hours, Minutes, Months, Seconds, TimeUnitField};

const NANOS: u64 = 1_000_000;
const SECONDS: u64 = 1_000;

pub struct NextAfterQuery {
    initial_datetime: DateTime<Utc>,
    first_month: bool,
    first_day_of_month: bool,
    first_hour: bool,
    first_minute: bool,
    first_second: bool,
}

impl NextAfterQuery {
    pub fn from(after: &u64) -> NextAfterQuery {
        let rem = *after % NANOS;
        let secs = ((*after - rem) / (NANOS * SECONDS)) + 1;
        let initial_datetime = DateTime::from_naive_utc_and_offset(
            NaiveDateTime::from_timestamp_opt(secs as i64, 0).unwrap(),
            Utc,
        );
        NextAfterQuery {
            initial_datetime,
            first_month: true,
            first_day_of_month: true,
            first_hour: true,
            first_minute: true,
            first_second: true,
        }
    }

    pub fn year_lower_bound(&self) -> Ordinal {
        // Unlike the other units, years will never wrap around.
        self.initial_datetime.year() as u32
    }

    pub fn month_lower_bound(&mut self) -> Ordinal {
        if self.first_month {
            self.first_month = false;
            return self.initial_datetime.month();
        }
        Months::inclusive_min()
    }

    pub fn reset_month(&mut self) {
        self.first_month = false;
        self.reset_day_of_month();
    }

    pub fn day_of_month_lower_bound(&mut self) -> Ordinal {
        if self.first_day_of_month {
            self.first_day_of_month = false;
            return self.initial_datetime.day();
        }
        DaysOfMonth::inclusive_min()
    }

    pub fn reset_day_of_month(&mut self) {
        self.first_day_of_month = false;
        self.reset_hour();
    }

    pub fn hour_lower_bound(&mut self) -> Ordinal {
        if self.first_hour {
            self.first_hour = false;
            return self.initial_datetime.hour();
        }
        Hours::inclusive_min()
    }

    pub fn reset_hour(&mut self) {
        self.first_hour = false;
        self.reset_minute();
    }

    pub fn minute_lower_bound(&mut self) -> Ordinal {
        if self.first_minute {
            self.first_minute = false;
            return self.initial_datetime.minute();
        }
        Minutes::inclusive_min()
    }

    pub fn reset_minute(&mut self) {
        self.first_minute = false;
        self.reset_second();
    }

    pub fn second_lower_bound(&mut self) -> Ordinal {
        if self.first_second {
            self.first_second = false;
            return self.initial_datetime.second();
        }
        Seconds::inclusive_min()
    }

    pub fn reset_second(&mut self) {
        self.first_second = false;
    }
}
