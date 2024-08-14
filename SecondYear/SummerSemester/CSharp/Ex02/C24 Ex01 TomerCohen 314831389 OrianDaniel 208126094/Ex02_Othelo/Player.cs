using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex02_Othelo
{
    public class Player
    {
        private string m_Name;
        private char m_Color;
        private bool m_IsComputer;

        public Player (string i_Name, char i_Color, bool i_IsComputer)
        {
            m_Name = i_Name;
            m_Color = i_Color;
            m_IsComputer = i_IsComputer;
        }

        public string Name 
        { 
            get { return m_Name; } 
            set {  m_Name = value; }
        }

        public char Color 
        { 
            get { return m_Color; }
            set { m_Color = value; }
        }

        public bool IsComputer
        {
            get { return m_IsComputer; }
            set { m_IsComputer = value; }
        }
    }
}
