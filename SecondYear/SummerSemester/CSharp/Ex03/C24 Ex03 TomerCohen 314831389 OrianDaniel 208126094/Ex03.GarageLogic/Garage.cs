using System;
using System.Collections.Generic;

namespace Ex03.GarageLogic
{
    public class Garage
    {
        private readonly Dictionary<string, OwnerInfo> r_Vehicles;

        public Garage()
        {
            r_Vehicles = new Dictionary<string, OwnerInfo>();
        }

        public void AddVehicle(OwnerInfo i_OwnerInfo)
        {
            if (r_Vehicles.ContainsKey(i_OwnerInfo.Vehicle.LicenseNumber))
            {
                try
                {
                    i_OwnerInfo.VehicleState = eVehicleState.InRepair;
                }
                catch (Exception ex)
                {
                    throw new Exception(string.Format($"License number {i_OwnerInfo.Vehicle.LicenseNumber} is not in the system."), ex);
                }
            }
            else
            {

            }

        }
    }
}
