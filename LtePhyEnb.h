//
//                           SimuLTE
//
// This file is part of a software released under the license included in file
// "license.pdf". This license can be also found at http://www.ltesimulator.com/
// The above file and the present reference are part of the software itself,
// and cannot be removed from it.
//

#ifndef _LTE_AIRPHYENB_H_
#define _LTE_AIRPHYENB_H_

#include "stack/phy/layer/LtePhyBase.h"

class DasFilter;
class LteFeedbackPkt;

class LtePhyEnb : public LtePhyBase
{
    simsignal_t capacitySignal;
    cOutVector capacityVector;
    friend class DasFilter;
    double blerCurvesNewPhy_[3][15][49];
    double snrVector_[15];


  protected:
    /** Broadcast messages interval (equal to updatePos interval for mobility) */
    double bdcUpdateInterval_;

    /** Self message to trigger broadcast message sending for handover purposes */
    cMessage *bdcStarter_;

    /**
     * Pointer to the DAS Filter: used to call das function
     * when receiving broadcasts and to retrieve physical
     * antenna properties on packet reception
     */
    DasFilter* das_;
    //Used for PisaPhy feedback generator
    LteFeedbackDoubleVector fb_;

    virtual void initialize(int stage);

    virtual void handleSelfMessage(cMessage *msg);
    virtual void handleAirFrame(cMessage* msg);
    bool handleControlPkt(UserControlInfo* lteinfo, LteAirFrame* frame);
    bool computationalDroppingNaive();
    bool computationalDropping(double capacity);
    double* snrVectorTBler(TxMode txmode, double tbler, double snrVector_[]);
    double comp(double rate, double snr);
    int getCqi(TxMode txmode, double snr);
    double computationalLoad(std::vector<double> sinr, TxMode txmode, double snrVector[]);
    void handleFeedbackPkt(UserControlInfo* lteinfo, LteAirFrame* frame);
    virtual void requestFeedback(UserControlInfo* lteinfo, LteAirFrame* frame, LteFeedbackPkt* pkt);
    /**
     * Getter for the Das Filter
     */
    DasFilter* getDasFilter();
    // Feedback computation for PisaPhy
    LteFeedbackComputation* getFeedbackComputationFromName(std::string name, ParameterMap& params);
    void initializeFeedbackComputation();

  public:
    LtePhyEnb();
    virtual ~LtePhyEnb();

//        void setMicroTxPower();
};

#endif  /* _LTE_AIRPHYENB_H_ */
