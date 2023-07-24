package co.com.sura.entity.remision;

import co.com.sura.entity.maestro.Ciudad;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Remision {
    private String idRemision;
    private String estado;
    private Paciente paciente;
    private LocalDate fechaAdmision;
    private Integer edad;
    private String sexo;
    private float peso;
    private String tipoAfiliacion;
    private String nombreAseguradora;
    private String programa;
    private Ciudad ciudad;
    private String tipoAdmision;
    private String institucionRemite;
    private DatosAtencionPaciente datosAtencionPaciente;
    private List<Diagnostico> diagnosticos;
}
