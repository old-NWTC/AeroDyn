#!/usr/bin/env python
# encoding: utf-8

import numpy as np
from ccblade import CCAirfoil, CCBlade
import array
import os
from myutilities import plt
import matplotlib.pyplot as plt
import scipy.io as sio


# read eng file
def get_eng_data(fname, nsamp, nchan):

    fid = open(fname, 'rb')

    data = np.zeros((nsamp, nchan))

    for i in range(nsamp):

        # read data and pack in array
        temp = array.array('f')
        temp.fromfile(fid, nchan)

        data[i, :] = temp

        # read error bit
        temp = array.array('B')
        temp.fromfile(fid, 1)

    fid.close()

    return data



def get_loads(Uinf, yaw, azimuth, yawcorrection=False, coupled=False, return_bounds=False, return_az_variation=False,
        station_idx=0):

    fname = 'H%02d%04d0' % (Uinf, yaw)

    fid = open(fname + '.HD1', 'r')
    fid.readline()
    parts = fid.readline().split(',')
    nchan = int(parts[0])
    nsamp = int(parts[1])
    fid.close()

    th_idx = np.zeros(5)
    tq_idx = np.zeros(5)
    q_idx = np.zeros(5)

    with open(fname + '.HD1', 'r') as inF:
        for num, line in enumerate(inF):
            if 'Thrust coefficient at 30% span' in line:
                th_idx[0] = int(num) - 2
            elif 'Thrust coefficient at 47% span' in line:
                th_idx[1] = int(num) - 2
            elif 'Thrust coefficient at 63% span' in line:
                th_idx[2] = int(num) - 2
            elif 'Thrust coefficient at 80% span' in line:
                th_idx[3] = int(num) - 2
            elif 'Thrust coefficient at 95% span' in line:
                th_idx[4] = int(num) - 2
            if 'Torque coefficient at 30% span' in line:
                tq_idx[0] = int(num) - 2
            elif 'Torque coefficient at 47% span' in line:
                tq_idx[1] = int(num) - 2
            elif 'Torque coefficient at 63% span' in line:
                tq_idx[2] = int(num) - 2
            elif 'Torque coefficient at 80% span' in line:
                tq_idx[3] = int(num) - 2
            elif 'Torque coefficient at 95% span' in line:
                tq_idx[4] = int(num) - 2
            elif 'QNORM30' in line:
                q_idx[0] = int(num) - 2
            elif 'QNORM47' in line:
                q_idx[1] = int(num) - 2
            elif 'QNORM63' in line:
                q_idx[2] = int(num) - 2
            elif 'QNORM80' in line:
                q_idx[3] = int(num) - 2
            elif 'QNORM95' in line:
                q_idx[4] = int(num) - 2
            elif 'Blade azimuth angle' in line:
                az_idx = int(num) - 2
            elif 'CONE' in line:
                cone_idx = int(num) - 2
            elif 'RHOTUN' in line:
                rho_idx = int(num) - 2
            elif 'RPM' in line:
                rpm_idx = int(num) - 2


    data = get_eng_data(fname + '.ENG', nsamp, nchan)


    az_vec = data[:, az_idx]
    idx = np.nonzero(np.abs(az_vec - azimuth) < 1)

    cone_vec = data[:, cone_idx]
    cone = np.mean(cone_vec[idx])

    rho_vec = data[:, rho_idx]
    rho = np.mean(rho_vec[idx])

    rpm_vec = data[:, rpm_idx]
    rpm = np.mean(rpm_vec[idx])

    r_exp = [0.3, 0.47, 0.63, 0.8, 0.95]
    c_exp = [0.711, 0.62492758, 0.54337911, 0.457, 0.381]
    q_exp = np.zeros(5)
    cn_exp = np.zeros(5)
    ct_exp = np.zeros(5)

    th_mean = np.zeros(5)
    th_max = np.zeros(5)
    th_min = np.zeros(5)
    tq_mean = np.zeros(5)
    tq_max = np.zeros(5)
    tq_min = np.zeros(5)


    th_vec_all = np.zeros((5, nsamp))
    tq_vec_all = np.zeros((5, nsamp))


    for i in range(5):
        cth_vec = data[:, th_idx[i]]
        ctq_vec = data[:, tq_idx[i]]
        q_vec = data[:, q_idx[i]]

        th_vec = cth_vec * q_vec * c_exp[i] / np.cos(np.radians(cone_vec))
        tq_vec = ctq_vec * q_vec * c_exp[i] / np.cos(np.radians(cone_vec))

        th_mean[i] = np.mean(th_vec[idx])
        th_max[i] = np.max(th_vec[idx])
        th_min[i] = np.min(th_vec[idx])

        tq_mean[i] = np.mean(tq_vec[idx])
        tq_max[i] = np.max(tq_vec[idx])
        tq_min[i] = np.min(tq_vec[idx])

        th_vec_all[i, :] = th_vec
        tq_vec_all[i, :] = tq_vec

    Np_exp = th_mean
    Tp_exp = tq_mean
    Np_min = th_min
    Tp_min = tq_min
    Np_max = th_max
    Tp_max = tq_max


    # geometry
    Rhub = 0.508
    Rtip = 5.029

    # 0.508,
    # r = np.array([0.660, 0.883, 1.008, 1.067, 1.133, 1.257, 1.343, 1.510, 1.648,
    #     1.952, 2.257, 2.343, 2.562, 2.867, 3.172, 3.185, 3.476, 3.781, 4.023, 4.086, 4.391,
    #     4.696, 4.780, 5.000])  # , 5.305, 5.532]

    # # 0.218,
    # chord = np.array([0.218, 0.183, 0.349, 0.441, 0.544, 0.737, 0.728, 0.711, 0.697,
    #     0.666, 0.636, 0.627, 0.605, 0.574, 0.543, 0.542, 0.512, 0.482, 0.457, 0.451, 0.420,
    #     0.389, 0.381, 0.358])  # , 0.328, 0.305])

    # # 0.0,
    # theta = np.array([0.0, 0.0, 6.7, 9.9, 13.4, 20.04, 18.074, 14.292, 11.909, 7.979,
    #     5.308, 4.715, 3.425, 2.083, 1.15, 1.115, 0.494, -0.015, -0.381, -0.475, -0.920,
    #     -1.352, -1.469, -1.775])  # , -2.191, -2.5])

    # af_idx = [0, 0, 0, 0, 0, 3, 3, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7]



    r = np.array([0.6060, 0.8801, 1.2321, 1.5087, 1.7099, 1.9278, 2.1457, 2.3469,
        2.5480, 2.7660, 2.9840, 3.1850, 3.3862, 3.6041, 3.8220, 4.0232, 4.2244,
        4.4004, 4.5764, 4.7776, 4.9536])

    chord = np.array([0.219, 0.181, 0.714, 0.711, 0.691, 0.668, 0.647, 0.627, 0.606,
        0.584, 0.561, 0.542, 0.522, 0.499, 0.478, 0.457, 0.437, 0.419, 0.401, 0.381, 0.363])

    theta = np.array([0.000, -0.098, 19.423, 14.318, 10.971, 8.244, 6.164, 4.689, 3.499,
        2.478, 1.686, 1.115, 0.666, 0.267, -0.079, -0.381, -0.679, -0.933, -1.184, -1.466, -1.711])

    af_idx = [0, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7]




    B = 2  # number of blades

    # # atmosphere
    # rho = 1.225
    # rho = 1.224385
    mu = 1.81206e-5

    # import os
    afinit = CCAirfoil.initFromAerodynFile  # just for shorthand
    basepath = 'af_files2' + os.path.sep


    airfoil_types = [0]*8
    airfoil_types[0] = afinit(basepath + 'cylinder.dat')
    airfoil_types[1] = afinit(basepath + 'S809_CLN_129.dat')  # .648741
    airfoil_types[2] = afinit(basepath + 'S809_CLN_185.dat')  # .930365
    airfoil_types[3] = afinit(basepath + 'S809_CLN_242.dat')  # 1.217018
    airfoil_types[4] = afinit(basepath + 'S809_CLN_298.dat')  # 1.498642
    airfoil_types[5] = afinit(basepath + 'S809_CLN_354.dat')  # 1.780266
    airfoil_types[6] = afinit(basepath + 'S809_CLN_410.dat')  # 2.06189
    airfoil_types[7] = afinit(basepath + 'S809_CLN_Outboard.dat')

    # ere is a cylindrical section at the root that extends from 0.508 to 0.883 m. The airfoil transition begins at approximately the 0.883- m radial station.


    af = [0]*len(r)
    for i in range(len(r)):
        af[i] = airfoil_types[af_idx[i]]


    tilt = 0.0
    precone_ccblade = -cone
    yaw_ccblade = yaw
    shearExp = 0.05
    hubHt = 12.192
    nSector = 8


    # create CCBlade object
    aeroanalysis = CCBlade(r, chord, theta, af, Rhub, Rtip, B, rho, mu,
                           precone_ccblade, tilt, yaw_ccblade, shearExp, hubHt, nSector,
                           yawcorrection=yawcorrection, coupled=coupled)


    # # set conditions
    # Uinf = 10.0
    # tsr = 7.55
    pitch = 3.0
    Omega = rpm  # Uinf*tsr/Rtip * 30.0/pi  # convert to RPM
    # azimuth_ccblade = (360.0 - azimuth) % 360  # opposite definition
    azimuth_ccblade = azimuth

    # evaluate distributed loadsp
    Np, Tp = aeroanalysis.distributedAeroLoads(Uinf, Omega, pitch, azimuth_ccblade)

    q = 0.5*rho*(Uinf + Omega*r*3.14/30)**2
    q_exp = np.interp(r_exp, r/Rtip, q)

    # cn = Np / (q * chord)
    # ct = Tp / (q * chord)

    # cn_exp = th_mean / (q_exp * c_exp)
    # ct_exp = tq_mean / (q_exp * c_exp)

    rstar = r/Rtip

    if return_bounds:
        return rstar, Np, Tp, r_exp, Np_exp, Tp_exp, Np_min, Tp_min, Np_max, Tp_max

    elif return_az_variation:
        Np_vec_exp = th_vec_all[station_idx, :]
        Tp_vec_exp = tq_vec_all[station_idx, :]

        n = 200
        az_vec_sim = np.linspace(0, 360, n)
        Np_vec = np.zeros(n)
        Tp_vec = np.zeros(n)

        for iii in range(n):
            Np, Tp = aeroanalysis.distributedAeroLoads(Uinf, Omega, pitch, az_vec_sim[iii])
            Np_vec[iii] = np.interp(r_exp[station_idx], r/Rtip, Np)
            Tp_vec[iii] = np.interp(r_exp[station_idx], r/Rtip, Tp)

        return az_vec, Np_vec_exp, Tp_vec_exp, az_vec_sim, Np_vec, Tp_vec

    else:
        return rstar, Np, Tp, r_exp, Np_exp, Tp_exp







saveit = False
showit = True
sweep_type = 'yaw'



if sweep_type == 'Uinf':
    yaw = 0.0
    azimuth = 90
    for Uinf in [5, 7, 10]:

        rstar, Np, Tp, r_exp, Np_exp, Tp_exp = get_loads(Uinf, yaw, azimuth)
        # rstar, Np, Tp, r_exp, Np_exp, Tp_exp, Np_min, Tp_min, Np_max, Tp_max = \
        #     get_loads(Uinf, yaw, azimuth, return_bounds=True)
        # # rstar2, Np2, Tp2, r_exp2, Np_exp2, Tp_exp2 = get_loads(Uinf, yaw, 90)
        # rstar3, Np3, Tp3, r_exp3, Np_exp3, Tp_exp3 = get_loads(Uinf, yaw, 180)
        # # rstar4, Np4, Tp4, r_exp4, Np_exp4, Tp_exp4 = get_loads(Uinf, yaw, 270)

        plt.figure()
        # 348ABD, A60628
        plt.plot(rstar, Tp, color='k', label='lead-lag')  # Py
        plt.plot(rstar, Np, color='r', label='flapwise')  # Px
        # plt.plot(rstar, Tp2, '--', color='k', label='lead-lag')  # Py
        # plt.plot(rstar, Np2, '--', color='r', label='flapwise')  # Px
        # plt.plot(rstar, Tp3, '.', color='k', label='lead-lag')  # Py
        # plt.plot(rstar, Np3, '.', color='r', label='flapwise')  # Px
        # plt.plot(rstar, Tp4, '*', color='k', label='lead-lag')  # Py
        # plt.plot(rstar, Np4, '*', color='r', label='flapwise')  # Px
        # plt.errorbar(r_exp, Tp_exp, yerr=[Tp_exp - Tp_min, Tp_max - Tp_exp], fmt='ko')
        # plt.errorbar(r_exp, Np_exp, yerr=[Np_exp - Np_min, Np_max - Np_exp], fmt='ro')
        plt.plot(r_exp, Tp_exp, 'o', color='k')
        plt.plot(r_exp, Np_exp, 'o', color='r')
        # plt.plot(r_exp, Tp_exp2, 'o', color='k')
        # plt.plot(r_exp, Np_exp2, 'o', color='r')
        # plt.plot(r_exp, Tp_exp3, 'o', color='k')
        # plt.plot(r_exp, Np_exp3, 'o', color='r')
        # plt.plot(r_exp, Tp_exp4, 'o', color='k')
        # plt.plot(r_exp, Np_exp4, 'o', color='r')
        plt.xlabel('radius fraction')
        plt.ylabel('force per unit length (N/m)')
        plt.legend(loc='upper left')
        plt.ylim([-50, 350])
        savename = 'Uinf_' + str(Uinf)

        if saveit:
            plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.pdf')

        if showit:
            plt.show()


if sweep_type == 'yaw':
    Uinf = 7.0
    azimuth = 90.0
    for yaw in [0.0, 20.0, 40.0, 60.0, 75.0, 90.0]:

        rstar, Np, Tp, r_exp, Np_exp, Tp_exp = get_loads(Uinf, yaw, azimuth, yawcorrection=False)
        rstar_pp, Np_pp, Tp_pp, r_exp_pp, Np_exp_pp, Tp_exp_pp = get_loads(Uinf, yaw, azimuth, yawcorrection=True)
        rstar_cp, Np_cp, Tp_cp, r_exp_cp, Np_exp_cp, Tp_exp_cp = get_loads(Uinf, yaw, azimuth, coupled=True)

        plt.figure()
        # 348ABD, A60628
        plt.plot(rstar, Tp, color='k', label='lead-lag')  # Py
        plt.plot(rstar, Np, color='r', label='flapwise')  # Px
        plt.plot(rstar_pp, Tp_pp, '--', color='k')
        plt.plot(rstar_pp, Np_pp, '--', color='r')
        # plt.plot(rstar_cp, Tp_cp, '.', color='k')
        # plt.plot(rstar_cp, Np_cp, '.', color='r')
        plt.plot(r_exp, Tp_exp, 'o', color='k')
        plt.plot(r_exp, Np_exp, 'o', color='r')
        plt.xlabel('radius fraction')
        plt.ylabel('force per unit length (N/m)')
        plt.legend(loc='upper left')
        plt.ylim([-150, 250])
        savename = 'yaw_' + str(int(yaw)) + '_az_' + str(int(azimuth))

        if saveit:
            plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.pdf')

        if showit:
            plt.show()

elif sweep_type == 'stations':

    Uinf = 7.0
    yaw = 40.0
    azimuth = 0.0  # irrelevant
    for station_idx in [0, 1, 2, 3, 4]:
    # for station_idx in [0, 2, 4]:

        az_vec_exp, Np_vec_exp, Tp_vec_exp, az_vec, Np_vec, Tp_vec = get_loads(Uinf, yaw, azimuth,
            return_az_variation=True, station_idx=station_idx)
        az_vec_exp, Np_vec_exp, Tp_vec_exp, az_vec_pp, Np_vec_pp, Tp_vec_pp = get_loads(Uinf, yaw, azimuth,
            return_az_variation=True, station_idx=station_idx, yawcorrection=True)
        az_vec_exp, Np_vec_exp, Tp_vec_exp, az_vec_cp, Np_vec_cp, Tp_vec_cp = get_loads(Uinf, yaw, azimuth,
            return_az_variation=True, station_idx=station_idx, coupled=True)

        plt.figure()
        plt.plot(az_vec_exp, Np_vec_exp, '.', color='#348ABD')
        plt.plot(az_vec, Np_vec, 'r')
        plt.plot(az_vec_pp, Np_vec_pp, 'r--')

        r_exp = [0.3, 0.47, 0.63, 0.8, 0.95]
        savename = 'station_Np_' + str(int(r_exp[station_idx]*100))


        if saveit:
            plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.png')

        plt.figure()
        plt.plot(az_vec_exp, Tp_vec_exp, '.', color='#348ABD')
        plt.plot(az_vec, Tp_vec, 'k')
        plt.plot(az_vec_pp, Tp_vec_pp, 'k--')

        savename = 'station_Tp_' + str(int(r_exp[station_idx]*100))


        if saveit:
            plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.png')

        if showit:
            plt.show()



# for iiter in iteration:

#     if sweep_type == 'Uinf':
#         Uinf = iiter
#     elif sweep_type == 'yaw1' or sweep_type == 'yaw2' or sweep_type == 'yaw3':
#         yaw = iiter
#     elif sweep_type == 'stations':
#         station_idx = iiter





    # Uinf = 7.0
    # # yaw = 0.0
    # yawcorrection = True
    # coupled = False

    # azimuth = 90
    # station_idx = 2


    # Npo = np.array([2.09286218286, 2.03274337162, 4.11699494024, 5.34830988111, 6.80478278417, 43.7262688969, 49.4131091837, 61.5716886696, 71.5372782961, 93.0903764118, 110.553594646, 116.513039868, 131.827302104, 152.511511341, 171.679104458, 172.550462649, 188.494942912, 203.015320841, 212.021817167, 214.182855915, 220.601960628, 215.018468393, 207.72321362, 150.767800537])

    # plot
    # import matplotlib.pyplot as plt



    # if not plot_stations:
    #     plt.figure()
    #     plt.plot(rstar, Tp, 'k', label='lead-lag')  # Py
    #     plt.plot(rstar, Np, 'r', label='flapwise')  # Px
    #     # from myutilities import printArray
    #     # printArray(Np, name='Np')
    #     # plt.errorbar(r_exp, tq_mean, yerr=[tq_mean-tq_min, tq_max-tq_mean], fmt='ko')
    #     # plt.errorbar(r_exp, th_mean, yerr=[th_mean-th_min, th_max-th_mean], fmt='ro')
    #     plt.plot(r_exp, tq_mean, 'ko')
    #     plt.plot(r_exp, th_mean, 'ro')

    #     # plt.plot(rstar, ct, 'k', label='$c_t$')
    #     # plt.plot(rstar, cn, 'r', label='$c_n$')
    #     # plt.plot(r_exp, ct_exp, 'ko')
    #     # plt.plot(r_exp, cn_exp, 'ro')
    #     plt.xlabel('radius fraction')
    #     plt.ylabel('force per unit length (N/m)')
    #     # plt.ylabel('force coefficients')
    #     plt.legend(loc='upper left')

    #     if sweep_type == 'Uinf':
    #         plt.ylim([-50, 350])
    #         savename = 'Uinf_' + str(Uinf)
    #     elif sweep_type == 'yaw1':
    #         plt.ylim([-100, 250])
    #         savename = 'yaw_vel_' + str(int(yaw))
    #     elif sweep_type == 'yaw2':
    #         plt.ylim([-100, 250])
    #         savename = 'yaw_pitt_' + str(int(yaw))

    #     if saveit:
    #         plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.pdf')

    # else:

    #     n = 200
    #     az_vec2 = np.linspace(0, 360, n)
    #     Np_az_vel = np.zeros(n)
    #     Tp_az_vel = np.zeros(n)
    #     Np_az_pitt = np.zeros(n)
    #     Tp_az_pitt = np.zeros(n)

    #     yawcorrection = False
    #     aeroanalysis_vel = CCBlade(r, chord, theta, af, Rhub, Rtip, B, rho, mu,
    #                        precone_ccblade, tilt, yaw_ccblade, shearExp, hubHt, nSector,
    #                        yawcorrection=yawcorrection, coupled=coupled)

    #     yawcorrection = True
    #     aeroanalysis_pitt = CCBlade(r, chord, theta, af, Rhub, Rtip, B, rho, mu,
    #                        precone_ccblade, tilt, yaw_ccblade, shearExp, hubHt, nSector,
    #                        yawcorrection=yawcorrection, coupled=coupled)


    #     for i in range(n):
    #         Np_vel, Tp_vel = aeroanalysis_vel.distributedAeroLoads(Uinf, Omega, pitch, az_vec2[i])
    #         Np_pitt, Tp_pitt = aeroanalysis_pitt.distributedAeroLoads(Uinf, Omega, pitch, az_vec2[i])
    #         Np_az_vel[i] = np.interp(r_exp[station_idx], r/Rtip, Np_vel)
    #         Tp_az_vel[i] = np.interp(r_exp[station_idx], r/Rtip, Tp_vel)
    #         Np_az_pitt[i] = np.interp(r_exp[station_idx], r/Rtip, Np_pitt)
    #         Tp_az_pitt[i] = np.interp(r_exp[station_idx], r/Rtip, Tp_pitt)

    #     plt.figure()
    #     plt.plot(az_vec, th_vec_all[station_idx, :], '.')
    #     plt.plot(az_vec2, Np_az_vel)
    #     plt.plot(az_vec2, Np_az_pitt)

    #     savename = 'station_Np_' + str(int(r_exp[station_idx]*100))

    #     if saveit:
    #         plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.png')

    #     plt.figure()
    #     plt.plot(az_vec, tq_vec_all[station_idx, :], '.')
    #     plt.plot(az_vec2, Tp_az_vel)
    #     plt.plot(az_vec2, Tp_az_pitt)

    #     savename = 'station_Tp_' + str(int(r_exp[station_idx]*100))

    #     if saveit:
    #         plt.save('/Users/andrewning/Dropbox/NREL/AeroDyn_Skew/images/' + savename + '.png')



    # if showit:
    #     plt.show()
