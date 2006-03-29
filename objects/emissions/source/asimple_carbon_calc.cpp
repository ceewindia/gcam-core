/*! 
 * \file asimple_carbon_calc.cpp
 * \ingroup Objects
 * \brief ASimpleCarbonCalc class source file.
 * \author James Blackwood
 */
#include "emissions/include/asimple_carbon_calc.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include <cassert>
#include <cfloat>

using namespace std;
using namespace xercesc;

/*! \brief Constructor.
* \author James Blackwood
*/
ASimpleCarbonCalc::ASimpleCarbonCalc() : mCurrentEmissions( getStartYear(), getEndYear() ),
                                         mCalculated( getStartYear(), getEndYear() ),
                                         mTotalEmissions( getStartYear(), getEndYear() ),
                                         mIsFirstTime( false )
{
}

//! Default destructor
ASimpleCarbonCalc::~ASimpleCarbonCalc() {
}

void ASimpleCarbonCalc::calc( const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    const int timeStep = modeltime->gettimestep( aPeriod );
    const int calcYear = modeltime->getper_to_yr( aPeriod );

    // Find any years up to the current period that have not been calculated.
    for( int i = getStartYear(); i <= calcYear - timeStep; ++i ){
        if( !mCalculated[ i ] ){
            mCalculated[ i ] = true;
            // Calculate above and below ground land use change emissions.
            calcAboveGroundCarbonEmission( i, false );
            calcBelowGroundCarbonEmission( i, false );
        }
    }

    // If the period was already calculated, remove the previously added
    // emissions or uptake from the totals.
    if( mIsFirstTime[ aPeriod ] ){
        for( unsigned int i = getStartYear(); i <= getEndYear(); ++i ){
            mTotalEmissions[ i ] -= mCurrentEmissions[ i ];
            
            // Clear the current emissions for the year.
            mCurrentEmissions[ i ] = 0;
        }
    }
    else {
        mIsFirstTime[ aPeriod ] = false;

        // Clear the current emissions as this is a new year.
        mCurrentEmissions.assign( mCurrentEmissions.size(), 0.0 );
    }

    // Calculate the present period.
    for( int year = calcYear - timeStep + 1; year <= calcYear; ++year ){
        mCalculated[ year ] = true;
        calcAboveGroundCarbonEmission( year, true );
        calcBelowGroundCarbonEmission( year, true );
    }
}

double ASimpleCarbonCalc::getNetLandUseChangeEmission( const int aYear ) const {
    assert( mCalculated[ aYear ] );
    return mTotalEmissions[ aYear ];
}

double ASimpleCarbonCalc::getNetTerrestrial( const int aYear ) const {
    // The simple carbon calculator does not implement this function as it is
    // not detailed enough to include a full carbon cycle.
    return DBL_MAX;
}

void ASimpleCarbonCalc::setTotalLandUse( const double aLandUse, const int aPeriod ) {
    mLandUse[ aPeriod ] = aLandUse;
}

double ASimpleCarbonCalc::getPotentialAboveGroundCarbon( const int aYear ) const {
    return getPotentialAboveGroundCarbonPerLandArea( aYear ) * mLandUse[ aYear ];
}

double ASimpleCarbonCalc::getPotentialBelowGroundCarbon( const int aYear ) const {
    return getPotentialBelowGroundCarbonPerLandArea( aYear ) * mLandUse[ aYear ];
}

void ASimpleCarbonCalc::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitCarbonCalc( this, aPeriod );
    aVisitor->endVisitCarbonCalc( this, aPeriod );
}

/*!
 * \brief Get the land usage for a year.
 * \param aYear Year.
 * \return Land usage for the year.
 */
double ASimpleCarbonCalc::getLandUse( const int aYear ) const {
    return interpYearHelper( mLandUse, aYear );
}

/*!
 * \brief Calculate the emission from above ground carbon for a given year.
 * \details Above ground carbon is emitted as a pulse.
 * \param aYear Year.
 * \param aIsCurrentYear Whether the year being calculated is the current year.
 */
void ASimpleCarbonCalc::calcAboveGroundCarbonEmission( const unsigned int aYear,
                                                       const bool aIsCurrentYear )
{
    // Above ground carbon currently always contains the maximum potential
    // amount of carbon.
    double prevCarbon = 0;
    if( aYear > getStartYear() ){
        prevCarbon = getLandUse( aYear - 1 )
                     * getPotentialAboveGroundCarbonPerLandArea( aYear - 1 );
    }
    
    double currCarbon = getLandUse( aYear )
                        * getPotentialAboveGroundCarbonPerLandArea( aYear );
    
    // Add a positive emission if the previous year contained more carbon
    // than the current year. If the previous year contains less carbon than the
    // current year than a negative emission representing uptake has occurred.
    mTotalEmissions[ aYear ] += ( prevCarbon - currCarbon );

    // If this is the current year being calculated store the emission
    // seperately so it can be removed in future iterations.
    if( aIsCurrentYear ){
        mCurrentEmissions[ aYear ] += ( prevCarbon - currCarbon );
    }
}

/*!
* \brief Calculate the emission from below ground carbon for the given year.
* \details Below ground, or soil carbon, is not emitted as a pulse but at a
*          rate defined by an exponential decay function.
* \param aYear Year.
* \param aIsCurrentYear Whether the year being calculated is the current year.
*/
void ASimpleCarbonCalc::calcBelowGroundCarbonEmission( const unsigned int aYear,
                                                       const bool aIsCurrentYear )
{
    // Calculate the total emission which will be spread across the full
    // emission time.
    double soilCarbonPrev = 0;
    if( aYear > getStartYear() ){
        soilCarbonPrev = getLandUse( aYear - 1 )
                         * getPotentialBelowGroundCarbonPerLandArea( aYear - 1 );
    }

    double soilCarbonCurr = getLandUse( aYear )
                            * getPotentialBelowGroundCarbonPerLandArea( aYear );

    double carbonDifference = soilCarbonCurr - soilCarbonPrev;

    // If the carbon content is equivalent than there is no carbon difference in
    // the current year.
    if( util::isEqual( carbonDifference, 0.0 ) ){
        return;
    }

    // Set emissions from now until the end of the model.
    for( unsigned int year = aYear; year <= getEndYear(); ++year ){
        // Calculate the future emissions for the year defined by the function:
        // E = deltaC/Tau * e^(-t/Tau)
        double futureAnnualEmiss = carbonDifference / getSoilTimeScale()
                                   * ( 1 - exp( -1 * double( year - aYear )
                                   / getSoilTimeScale() ) );

        // Only store annual emissions values if this is not a historical
        // emissions calculation. Historical emissions calculations only occur
        // once, unlike current emissions calculations which need to remove the
        // effect of the previous iteration.
        if( aIsCurrentYear ){
            mCurrentEmissions[ year ] += futureAnnualEmiss;
        }

        // Add to the total carbon emission for the year. This will be the sum
        // of the effects of all carbon emissions for the previous years.
        mTotalEmissions[ year ] += futureAnnualEmiss;
    }
}

/*! \brief A static function to return the starting year to index the arrays.
*
* \todo This is not the best approach because it is hard coded.
* \author James Blackwood
* \return The startYear.
*/
const unsigned int ASimpleCarbonCalc::getStartYear() const {
	return 1960;
}

/*!
 * \brief Return the last year of the climate calculation.
 * \todo Make this value dynamic.
 * \author James Blackwood
 * \return The last year of the climate calculation.
 */
const unsigned int ASimpleCarbonCalc::getEndYear() const {
	return 2095;
}

/*
 * \brief Returns a parameter which defines the time scale for the soil
 *        emissions decay function.
 * \return Soil decay function time scale parameter.
 * \todo This should be dynamic by land type.
 */
double ASimpleCarbonCalc::getSoilTimeScale() const {
    return 40;
}

/*!
 * \brief Helper function to interpolate a value for a year from a PeriodVector.
 * \details Calculates a linearly interpolated value for the year. If the year
 *          is before the first period of the vector, the first value is
 *          returned. If the year is after the last period of the vector, the
 *          last value is used. Otherwise a value is linearly interpolated
 *          between the nearest two periods.
 * \param aPeriodVector Vector from which to interpolate the value.
 * \param aYear Year for which to interpolate a value.
 * \return Interpolated value for the year.
 */
double ASimpleCarbonCalc::interpYearHelper( const objects::PeriodVector<double>& aPeriodVector,
                                            const int aYear )
{
    // If the year is before the first period of the model use the carbon
    // content in the base period.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear <= modeltime->getStartYear() ){
        return *aPeriodVector.begin();
    }

    // If the year is after the end period use the carbon content in the last
    // period.
    if( aYear > modeltime->getEndYear() ){
        return *aPeriodVector.last();
    }
    
    // Find the period containing aYear. This cannot be zero because the year
    // was already checked against the start year.
    int currPeriod = modeltime->getyr_to_per( aYear );

    // Find the last year of the current period.
    int lastYear = modeltime->getper_to_yr( currPeriod );

    // Find the first year of the current period.
    int firstYear = modeltime->getper_to_yr( currPeriod - 1 );

    // Interpolate the result.
    return util::linearInterpolateY( aYear, firstYear, lastYear,
                                     aPeriodVector[ currPeriod - 1 ],
                                     aPeriodVector[ currPeriod ] );
}