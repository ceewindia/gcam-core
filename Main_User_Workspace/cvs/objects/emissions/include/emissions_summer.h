#ifndef _EMISSIONS_SUMMER_H_
#define _EMISSIONS_SUMMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file emissions_summer.h
* \ingroup Objects
* \brief EmissionsSummer class header file.
* \author Josh Lurz
*/

#include "util/base/include/time_vector.h"
#include "util/base/include/default_visitor.h"
#include "util/base/include/value.h"

class AgSector;

/*! 
* \ingroup Objects
* \brief A class which sums emissions for a particular gas.
* \details 
* \author Josh Lurz
*/

class EmissionsSummer : public DefaultVisitor {
public:
    explicit EmissionsSummer( const std::string& aGHGName );

    virtual void startVisitGHG( const AGHG* aGHG,
                                const int aPeriod );
    
    // TODO: Remove this when the Fortran land allocator is removed.
    virtual void startVisitAgSector( const AgSector* aAgSector,
                                     const int aPeriod );

    virtual void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                       const int aPeriod );

    // Non-IVisitor interface methods.
    double getEmissions( const int aPeriod ) const;

    double areEmissionsSet( const int aPeriod ) const;
private:
    //! The name of the GHG being summed.
    const std::string mGHGName;

    //! The current sum.
    objects::PeriodVector<Value> mEmissionsByPeriod;
};

#endif // _EMISSIONS_SUMMER_H_