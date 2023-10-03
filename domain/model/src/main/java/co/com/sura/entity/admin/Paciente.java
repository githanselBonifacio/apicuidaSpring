package co.com.sura.entity.admin;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Paciente {
    private String tipoIdentificacion;
    private String numeroIdentificacion;
    private String nombres;
    private String apellidos;
    private String edad;
    private String sexo;
    private String peso;
    private String tipoAfiliacion;
    private String nombreAseguradora;
    private LocalDate fechaNacimiento;
    private Ubicacion ubicacion;
}
