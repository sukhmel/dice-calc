use crate::error::CalcError;
use crate::types::CalcResult;
use crate::types::Configuration;
use crate::types::NumValue;
use crate::types::Outcome;
use crate::types::Throw;
use crate::types::Value;
use num::BigUint;
use num::Zero;
use std::ops::Div;

impl Div for Configuration {
    type Output = CalcResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        let rhs_str = rhs.to_string();
        let self_str = self.to_string();

        let denom = match rhs
            .events
            .into_iter()
            .map(|outcome| {
                let Ok([value]) = TryInto::<[_; 1]>::try_into(outcome.throw.0) else {
                    return Err(format!(
                        "Can't divide, denominator not a set of single value outcomes"
                    ));
                };
                let Value::Numeric(value) = value else {
                    return Err(format!(
                        "Can't divide, denominator contains non-numerical values"
                    ));
                };
                if value.is_zero() {
                    return Err(format!("Can't divide by zero"));
                }
                Ok((value, outcome.count))
            })
            .collect::<Result<Vec<(NumValue, BigUint)>, String>>()
        {
            Ok(denom) => denom,
            Err(details) => {
                return Err(CalcError::UnexpectedArgument {
                    arg: rhs_str,
                    details,
                })
            }
        };
        let numer = match self
            .events
            .into_iter()
            .map(|outcome| {
                outcome
                    .throw
                    .0
                    .into_iter()
                    .map(|value| {
                        let Value::Numeric(value) = value else {
                            return Err(format!(
                                "Can't divide, numerator contains non-numerical values"
                            ));
                        };
                        Ok(value)
                    })
                    .collect::<Result<Vec<NumValue>, String>>()
                    .map(|values| (values, outcome.count))
            })
            .collect::<Result<Vec<(Vec<NumValue>, BigUint)>, String>>()
        {
            Ok(numer) => numer,
            Err(details) => {
                return Err(CalcError::UnexpectedArgument {
                    arg: self_str,
                    details,
                })
            }
        };
        let result = numer
            .into_iter()
            .flat_map(move |(events, num_count)| {
                denom
                    .clone()
                    .into_iter()
                    .map(move |(denom, denom_count)| Outcome {
                        throw: Throw::new(
                            events
                                .iter()
                                .map(|num| Value::Numeric(num.clone() / denom))
                                .collect(),
                        ),
                        count: num_count.clone() * denom_count,
                    })
            })
            .collect::<Vec<Outcome>>();
        let total = result
            .iter()
            .fold(BigUint::zero(), |acc, outcome| acc + outcome.count.clone());
        Ok(Configuration::new(result, total))
    }
}
