namespace Ex05_Othelo
{
    partial class FormGameSettings
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.buttonBoardSize = new System.Windows.Forms.Button();
            this.buttonPlayComputer = new System.Windows.Forms.Button();
            this.buttonPlayFriend = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // buttonBoardSize
            // 
            this.buttonBoardSize.Location = new System.Drawing.Point(12, 12);
            this.buttonBoardSize.Name = "buttonBoardSize";
            this.buttonBoardSize.Size = new System.Drawing.Size(262, 49);
            this.buttonBoardSize.TabIndex = 0;
            this.buttonBoardSize.Text = "Board Size: 6x6 (click to increase)";
            this.buttonBoardSize.UseVisualStyleBackColor = true;
            this.buttonBoardSize.Click += new System.EventHandler(this.buttonBoardSize_Click);
            // 
            // buttonPlayComputer
            // 
            this.buttonPlayComputer.Location = new System.Drawing.Point(12, 84);
            this.buttonPlayComputer.Name = "buttonPlayComputer";
            this.buttonPlayComputer.Size = new System.Drawing.Size(130, 49);
            this.buttonPlayComputer.TabIndex = 1;
            this.buttonPlayComputer.Text = "Play against the computer";
            this.buttonPlayComputer.UseVisualStyleBackColor = true;
            this.buttonPlayComputer.Click += new System.EventHandler(this.buttonPlayComputer_Click);
            // 
            // buttonPlayFriend
            // 
            this.buttonPlayFriend.Location = new System.Drawing.Point(148, 84);
            this.buttonPlayFriend.Name = "buttonPlayFriend";
            this.buttonPlayFriend.Size = new System.Drawing.Size(126, 49);
            this.buttonPlayFriend.TabIndex = 2;
            this.buttonPlayFriend.Text = "Play against your friend";
            this.buttonPlayFriend.UseVisualStyleBackColor = true;
            this.buttonPlayFriend.Click += new System.EventHandler(this.buttonPlayFriend_Click);
            // 
            // FormGameSettings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(286, 146);
            this.Controls.Add(this.buttonPlayFriend);
            this.Controls.Add(this.buttonPlayComputer);
            this.Controls.Add(this.buttonBoardSize);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.MinimizeBox = false;
            this.Name = "FormGameSettings";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "GameSettings";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button buttonBoardSize;
        private System.Windows.Forms.Button buttonPlayComputer;
        private System.Windows.Forms.Button buttonPlayFriend;
    }
}