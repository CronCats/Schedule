use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, multispace0};

use nom::combinator::{complete, eof, map, map_res, opt};
use nom::multi::separated_list1;
use nom::sequence::tuple;
use nom::IResult;

use std::iter::Iterator;
use std::str::{self, FromStr};

use crate::error::{Error, ErrorKind};
use crate::ordinal::*;
use crate::schedule::{Schedule, ScheduleFields};
use crate::specifier::*;
use crate::time_unit::*;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F, O, E: nom::error::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
{
    nom::sequence::delimited(multispace0, inner, multispace0)
}

impl FromStr for Schedule {
    type Err = Error;
    fn from_str(expression: &str) -> Result<Self, Self::Err> {
        match schedule(expression) {
            Ok((_, schedule_fields)) => {
                Ok(Schedule::new(String::from(expression), schedule_fields))
            } // Extract from nom tuple
            Err(_) => Err(ErrorKind::Expression("Invalid cron expression.".to_owned()).into()), //TODO: Details
        }
    }
}

impl ScheduleFields {
    pub fn from_field_list(fields: Vec<Field>) -> Result<ScheduleFields, Error> {
        let number_of_fields = fields.len();
        if number_of_fields != 6 && number_of_fields != 7 {
            return Err(ErrorKind::Expression(format!(
                "Expression has {} fields. Valid cron \
                 expressions have 6 or 7.",
                number_of_fields
            ))
            .into());
        }

        let mut iter = fields.into_iter();

        let seconds = Seconds::from_field(iter.next().unwrap())?;
        let minutes = Minutes::from_field(iter.next().unwrap())?;
        let hours = Hours::from_field(iter.next().unwrap())?;
        let days_of_month = DaysOfMonth::from_field(iter.next().unwrap())?;
        let months = Months::from_field(iter.next().unwrap())?;
        let days_of_week = DaysOfWeek::from_field(iter.next().unwrap())?;
        let years: Years = iter
            .next()
            .map(Years::from_field)
            .unwrap_or_else(|| Ok(Years::all()))?;

        Ok(ScheduleFields::new(
            seconds,
            minutes,
            hours,
            days_of_month,
            months,
            days_of_week,
            years,
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Field {
    pub specifiers: Vec<RootSpecifier>, // TODO: expose iterator?
}

trait FromField
where
    Self: Sized,
{
    //TODO: Replace with std::convert::TryFrom when stable
    fn from_field(field: Field) -> Result<Self, Error>;
}

impl<T> FromField for T
where
    T: TimeUnitField,
{
    fn from_field(field: Field) -> Result<T, Error> {
        if field.specifiers.len() == 1
            && field.specifiers.get(0).unwrap() == &RootSpecifier::from(Specifier::All)
        {
            return Ok(T::all());
        }
        let mut ordinals = OrdinalSet::new();
        for specifier in field.specifiers {
            let specifier_ordinals: OrdinalSet = T::ordinals_from_root_specifier(&specifier)?;
            for ordinal in specifier_ordinals {
                ordinals.insert(T::validate_ordinal(ordinal)?);
            }
        }
        Ok(T::from_ordinal_set(ordinals))
    }
}

fn ordinal(x: &str) -> IResult<&str, u32, nom::error::Error<&str>> {
    map_res(ws(digit1), u32::from_str)(x)
}

fn name(x: &str) -> IResult<&str, String, nom::error::Error<&str>> {
    map(ws(alpha1), String::from)(x)
}

fn point(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    map(ordinal, Specifier::Point)(x)
}

fn named_point(x: &str) -> IResult<&str, RootSpecifier, nom::error::Error<&str>> {
    map(name, RootSpecifier::NamedPoint)(x)
}

fn period(x: &str) -> IResult<&str, RootSpecifier, nom::error::Error<&str>> {
    map(
        tuple((specifier, tag("/"), ordinal)),
        |(start, _split, step)| RootSpecifier::Period(start, step),
    )(x)
}

fn period_with_any(x: &str) -> IResult<&str, RootSpecifier, nom::error::Error<&str>> {
    map(
        tuple((specifier_with_any, tag("/"), ordinal)),
        |(start, _split, step)| RootSpecifier::Period(start, step),
    )(x)
}

fn range(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    map(
        tuple((ordinal, tag("-"), ordinal)),
        |(start, _split, step)| Specifier::Range(start, step),
    )(x)
}

fn named_range(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    map(tuple((name, tag("-"), name)), |(start, _split, step)| {
        Specifier::NamedRange(start, step)
    })(x)
}

fn all(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    map(tag("*"), |_s: &str| Specifier::All)(x)
}

fn any(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    map(tag("?"), |_s: &str| Specifier::All)(x)
}

fn specifier(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    alt((all, range, point, named_range))(x)
}

fn specifier_with_any(x: &str) -> IResult<&str, Specifier, nom::error::Error<&str>> {
    alt((any, specifier))(x)
}

fn root_specifier(x: &str) -> IResult<&str, RootSpecifier, nom::error::Error<&str>> {
    alt((
        period,
        map(specifier, RootSpecifier::Specifier),
        named_point,
    ))(x)
}

fn root_specifier_with_any(x: &str) -> IResult<&str, RootSpecifier, nom::error::Error<&str>> {
    alt((
        period_with_any,
        map(specifier_with_any, RootSpecifier::from),
        named_point,
    ))(x)
}

fn root_specifier_list(x: &str) -> IResult<&str, Vec<RootSpecifier>, nom::error::Error<&str>> {
    ws(alt((
        separated_list1(tag(","), root_specifier),
        map(root_specifier, |spec| vec![spec]),
    )))(x)
}

fn root_specifier_list_with_any(
    x: &str,
) -> IResult<&str, Vec<RootSpecifier>, nom::error::Error<&str>> {
    ws(alt((
        separated_list1(tag(","), root_specifier_with_any),
        map(root_specifier_with_any, |spec| vec![spec]),
    )))(x)
}

fn field(x: &str) -> IResult<&str, Field, nom::error::Error<&str>> {
    map(root_specifier_list, |specifiers| Field { specifiers })(x)
}

fn field_with_any(x: &str) -> IResult<&str, Field, nom::error::Error<&str>> {
    map(root_specifier_list_with_any, |specifiers| Field {
        specifiers,
    })(x)
}

// 0 0 0 1 1 * *
fn shorthand_yearly(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(tag("@yearly"), |_tag| {
        ScheduleFields::new(
            Seconds::from_ordinal(0),
            Minutes::from_ordinal(0),
            Hours::from_ordinal(0),
            DaysOfMonth::from_ordinal(1),
            Months::from_ordinal(1),
            DaysOfWeek::all(),
            Years::all(),
        )
    })(x)
}

// 0 0 0 1 * * *
fn shorthand_monthly(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(tag("@monthly"), |_tag| {
        ScheduleFields::new(
            Seconds::from_ordinal(0),
            Minutes::from_ordinal(0),
            Hours::from_ordinal(0),
            DaysOfMonth::from_ordinal(1),
            Months::all(),
            DaysOfWeek::all(),
            Years::all(),
        )
    })(x)
}

// 0 0 0 * * 1 *
fn shorthand_weekly(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(tag("@weekly"), |_tag| {
        ScheduleFields::new(
            Seconds::from_ordinal(0),
            Minutes::from_ordinal(0),
            Hours::from_ordinal(0),
            DaysOfMonth::all(),
            Months::all(),
            DaysOfWeek::from_ordinal(1),
            Years::all(),
        )
    })(x)
}

// 0 0 0 * * * *
fn shorthand_daily(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(tag("@daily"), |_tag| {
        ScheduleFields::new(
            Seconds::from_ordinal(0),
            Minutes::from_ordinal(0),
            Hours::from_ordinal(0),
            DaysOfMonth::all(),
            Months::all(),
            DaysOfWeek::all(),
            Years::all(),
        )
    })(x)
}

// 0 0 * * * * *
fn shorthand_hourly(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(tag("@hourly"), |_tag| {
        ScheduleFields::new(
            Seconds::from_ordinal(0),
            Minutes::from_ordinal(0),
            Hours::all(),
            DaysOfMonth::all(),
            Months::all(),
            DaysOfWeek::all(),
            Years::all(),
        )
    })(x)
}

fn shorthand(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map(
        tuple((
            (alt((
                shorthand_yearly,
                shorthand_monthly,
                shorthand_weekly,
                shorthand_daily,
                shorthand_hourly,
            ))),
            complete(eof),
        )),
        |(schedule, _eof)| schedule,
    )(x)
}

fn longhand(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    map_res(
        tuple((
            field,
            field,
            field,
            field_with_any,
            field,
            field_with_any,
            opt(field),
            complete(eof),
        )),
        |(seconds, minutes, hours, days_of_month, months, days_of_week, years, _eof)| {
            let mut fields = vec![seconds, minutes, hours, days_of_month, months, days_of_week];
            if let Some(years) = years {
                fields.push(years);
            }
            ScheduleFields::from_field_list(fields)
        },
    )(x)
}

fn schedule(x: &str) -> IResult<&str, ScheduleFields, nom::error::Error<&str>> {
    alt((shorthand, longhand))(x)
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_nom_valid_number() {
        let expression = "  1997\n\n\t";
        let (input, s) = point(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, Specifier::Point(1997));
    }

    #[test]
    fn test_nom_invalid_point() {
        let expression = "a";
        assert!(point(expression).is_err());
    }

    #[test]
    fn test_nom_valid_named_point() {
        let expression = "WED";
        let (input, s) = named_point(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::NamedPoint(expression.to_owned()));
    }

    #[test]
    fn test_nom_invalid_named_point() {
        let expression = "8";
        assert!(named_point(expression).is_err());
    }

    #[test]
    fn test_nom_valid_period() {
        let expression = "1/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::Point(1), 2));
    }

    #[test]
    fn test_nom_invalid_period() {
        let expression = "Wed/4";
        assert!(period(expression).is_err());
    }

    #[test]
    fn test_nom_valid_number_list() {
        let expression = " \n1 , 2   *\n";
        let (input, f) = field(expression).unwrap();

        let (input, f1) = field(input).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            f,
            Field {
                specifiers: vec![
                    RootSpecifier::Specifier(Specifier::Point(1)),
                    RootSpecifier::Specifier(Specifier::Point(2)),
                ],
            }
        );
        assert_eq!(
            f1,
            Field {
                specifiers: vec![RootSpecifier::Specifier(Specifier::All)]
            }
        );
        let (input, f) = field_with_any(expression).unwrap();
        let (input, f1) = field_with_any(input).unwrap();

        assert!(input.is_empty());
        assert_eq!(
            f,
            Field {
                specifiers: vec![
                    RootSpecifier::Specifier(Specifier::Point(1)),
                    RootSpecifier::Specifier(Specifier::Point(2)),
                ],
            }
        );
        assert_eq!(
            f1,
            Field {
                specifiers: vec![RootSpecifier::Specifier(Specifier::All)]
            }
        );
    }

    #[test]
    fn test_nom_invalid_number_list() {
        let expression = ",1,2";
        assert!(field(expression).is_err());
        assert!(field_with_any(expression).is_err());
    }

    #[test]
    fn test_nom_field_with_any_valid_any() {
        let expression = "?";
        let (input, f) = field_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            f,
            Field {
                specifiers: vec![RootSpecifier::Specifier(Specifier::All)]
            }
        );
    }

    #[test]
    fn test_nom_field_invalid_any() {
        let expression = "?";
        assert!(field(expression).is_err());
    }

    #[test]
    fn test_nom_valid_range_field() {
        let expression = "1-4";
        let (input, f) = range(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(f, Specifier::Range(1, 4))
    }

    #[test]
    fn test_nom_valid_period_all() {
        let expression = "*/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::All, 2))
    }

    #[test]
    fn test_nom_valid_period_range() {
        let expression = "10-20/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::Range(10, 20), 2))
    }

    #[test]
    fn test_nom_valid_period_named_range() {
        let expression = "Mon-Thurs/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            s,
            RootSpecifier::Period(
                Specifier::NamedRange("Mon".to_owned(), "Thurs".to_owned()),
                2
            )
        );

        let expression = "February-November/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            s,
            RootSpecifier::Period(
                Specifier::NamedRange("February".to_owned(), "November".to_owned()),
                2
            )
        );
    }

    #[test]
    fn test_nom_valid_period_point() {
        let expression = "10/2";
        let (input, s) = period(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::Point(10), 2));
    }

    #[test]
    fn test_nom_invalid_period_any() {
        let expression = "?/2";
        assert!(period(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_period_named_point() {
        let expression = "Tues/2";
        assert!(period(expression).is_err());

        let expression = "February/2";
        assert!(period(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_period_specifier_range() {
        let expression = "10-12/*";
        assert!(period(expression).is_err());
    }

    #[test]
    fn test_nom_valid_period_with_any_all() {
        let expression = "*/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::All, 2));
    }

    #[test]
    fn test_nom_valid_period_with_any_range() {
        let expression = "10-20/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::Range(10, 20), 2));
    }

    #[test]
    fn test_nom_valid_period_with_any_named_range() {
        let expression = "Mon-Thurs/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            s,
            RootSpecifier::Period(
                Specifier::NamedRange("Mon".to_owned(), "Thurs".to_owned()),
                2
            )
        );

        let expression = "February-November/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            s,
            RootSpecifier::Period(
                Specifier::NamedRange("February".to_owned(), "November".to_owned()),
                2
            )
        );
    }

    #[test]
    fn test_nom_valid_period_with_any_point() {
        let expression = "10/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::Point(10), 2))
    }

    #[test]
    fn test_nom_valid_period_with_any_any() {
        let expression = "?/2";
        let (input, s) = period_with_any(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(s, RootSpecifier::Period(Specifier::All, 2))
    }

    #[test]
    fn test_nom_invalid_period_with_any_named_point() {
        let expression = "Tues/2";
        assert!(period_with_any(expression).is_err());

        let expression = "February/2";
        assert!(period_with_any(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_period_with_any_specifier_range() {
        let expression = "10-12/*";
        assert!(period_with_any(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_range_field() {
        let expression = "-4";
        assert!(range(expression).is_err());
    }

    #[test]
    fn test_nom_valid_named_range_field() {
        let expression = "TUES-THURS";
        let (input, s) = named_range(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            s,
            Specifier::NamedRange("TUES".to_owned(), "THURS".to_owned())
        );
    }

    #[test]
    fn test_nom_invalid_named_range_field() {
        let expression = "3-THURS";
        assert!(named_range(expression).is_err());
    }

    #[test]
    fn test_valid_shorthands() {
        let expression = "@hourly";
        let (input, hourly) = shorthand(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            hourly,
            ScheduleFields::new(
                Seconds::from_ordinal(0),
                Minutes::from_ordinal(0),
                Hours::all(),
                DaysOfMonth::all(),
                Months::all(),
                DaysOfWeek::all(),
                Years::all()
            )
        );

        let expression = "@daily";
        let (input, daily) = shorthand(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            daily,
            ScheduleFields::new(
                Seconds::from_ordinal(0),
                Minutes::from_ordinal(0),
                Hours::from_ordinal(0),
                DaysOfMonth::all(),
                Months::all(),
                DaysOfWeek::all(),
                Years::all()
            )
        );

        let expression = "@weekly";
        let (input, weekly) = shorthand(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            weekly,
            ScheduleFields::new(
                Seconds::from_ordinal(0),
                Minutes::from_ordinal(0),
                Hours::from_ordinal(0),
                DaysOfMonth::all(),
                Months::all(),
                DaysOfWeek::from_ordinal(1),
                Years::all()
            )
        );

        let expression = "@monthly";
        let (input, monthly) = shorthand(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            monthly,
            ScheduleFields::new(
                Seconds::from_ordinal(0),
                Minutes::from_ordinal(0),
                Hours::from_ordinal(0),
                DaysOfMonth::from_ordinal(1),
                Months::all(),
                DaysOfWeek::all(),
                Years::all()
            )
        );

        let expression = "@yearly";
        let (input, yearly) = shorthand(expression).unwrap();
        assert!(input.is_empty());
        assert_eq!(
            yearly,
            ScheduleFields::new(
                Seconds::from_ordinal(0),
                Minutes::from_ordinal(0),
                Hours::from_ordinal(0),
                DaysOfMonth::from_ordinal(1),
                Months::from_ordinal(1),
                DaysOfWeek::all(),
                Years::all()
            )
        );
    }

    #[test]
    fn test_invalid_shorthand() {
        // wrong format
        let expression = "@minutely";
        assert!(shorthand(expression).is_err());

        let expression = "bad_format";
        assert!(shorthand(expression).is_err());

        // with extra symbols
        let expression = "@yearly ";
        assert!(shorthand(expression).is_err());

        let expression = " @yearly";
        assert!(shorthand(expression).is_err());
    }

    #[test]
    fn test_valid_longhand_with_year() {
        let expression = "* * * * * * *";
        let res = longhand(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_valid_longhand_without_year() {
        let expression = "* * * * * *";
        let res = longhand(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_schedule() {
        let expression = "* * * * * *";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_invalid_schedule() {
        let expression = "* * * *";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_valid_seconds_list() {
        let expression = "0,20,40 * * * * *";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::from_ordinal_set(OrdinalSet::from([0, 20, 40])),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_seconds_range() {
        let expression = "0-40 * * * * *";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::from_ordinal_set(OrdinalSet::from_iter(0..=40)),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_seconds_mix() {
        let expression = "0-5,58 * * * * *";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::from_ordinal_set(OrdinalSet::from_iter(
                        (0..=5).chain(std::iter::once(58))
                    )),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_invalid_seconds_range() {
        let expression = "0-65 * * * * *";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_seconds_list() {
        let expression = "103,12 * * * * *";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_seconds_mix() {
        let expression = "0-5,102 * * * * *";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_valid_days_of_week_list() {
        let expression = "* * * * * MON,WED,FRI";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::from_ordinal_set(OrdinalSet::from([2, 4, 6])),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_invalid_days_of_week_list() {
        let expression = "* * * * * MON,TURTLE";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_valid_days_of_week_range() {
        let expression = "* * * * * MON-FRI";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::from_ordinal_set(OrdinalSet::from_iter(2..=6)),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_invalid_days_of_week_range() {
        let expression = "* * * * * BEAR-OWL";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_period_with_range_specifier() {
        let expression = "10-12/10-12 * * * * ?";
        // is_space
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_valid_days_of_month_any() {
        let expression = "* * * ? * *";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_days_of_week_any() {
        let expression = "* * * * * ?";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_days_of_month_any_days_of_week_specific() {
        let expression = "* * * ? * Mon,Thu";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::from_ordinal_set(OrdinalSet::from([2,5])),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_days_of_week_any_days_of_month_specific() {
        let expression = "* * * 1,2 * ?";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::from_ordinal_set(OrdinalSet::from([1,2])),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_valid_dom_and_dow_any() {
        let expression = "* * * ? * ?";
        let res = schedule(expression).unwrap();
        assert_eq!(
            res,
            (
                "",
                ScheduleFields::new(
                    Seconds::all(),
                    Minutes::all(),
                    Hours::all(),
                    DaysOfMonth::all(),
                    Months::all(),
                    DaysOfWeek::all(),
                    Years::all()
                )
            )
        )
    }

    #[test]
    fn test_nom_invalid_other_fields_any() {
        let expression = "? * * * * *";
        assert!(schedule(expression).is_err());

        let expression = "* ? * * * *";
        assert!(schedule(expression).is_err());

        let expression = "* * ? * * *";
        assert!(schedule(expression).is_err());

        let expression = "* * * * ? *";
        assert!(schedule(expression).is_err());
    }

    #[test]
    fn test_nom_invalid_trailing_characters() {
        let expression = "* * * * * *foo *";
        assert!(schedule(expression).is_err());

        let expression = "* * * * * * * foo";
        assert!(schedule(expression).is_err());
    }
}
