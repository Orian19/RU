using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class OwnerInfo
    {
        private readonly string r_OwnerName;
        private readonly string r_TelephoneNumber;
        private readonly Vehicle m_Vehicle;
        private eVehicleState m_VehicleState;

        public OwnerInfo(string i_OwnerName, string i_TelephoneNumber, eVehicleState i_VehicleState, Vehicle i_Vehicle)
        {
            r_OwnerName = i_OwnerName;
            r_TelephoneNumber = i_TelephoneNumber;
            m_VehicleState = i_VehicleState;
            m_Vehicle = i_Vehicle;
        }

        public string OwnerName
        {
            get { return r_OwnerName; }
        }

        public string TelephoneNumber
        {
            get { return r_TelephoneNumber; }
        }

        public Vehicle Vehicle
        {
            get { return m_Vehicle; }
        }

        public eVehicleState VehicleState
        {
            get { return m_VehicleState; }
            set { m_VehicleState = value; }
        }

        public override string ToString()
        {
            return string.Format(
@"
License Number: {0}
Model Name: {1}
Owner Name: {2}
Vehicle State: {3}
Wheels Info: {4}
Energy State + Type: {5}
More Info: {6}
", m_Vehicle.ModelName, r_OwnerName, m_VehicleState, string.Join(", ", m_Vehicle.Wheels), energy, moreInfo);
        }
    }
}
