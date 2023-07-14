package co.com.sura.dto.remision;

import co.com.sura.entity.remision.Diagnostico;
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
public class RemisionRequest {
    private String idRemision;
    private String estado;
    private TipoIdentificacionRequest tipoIdentificacion;
    private String numeroIdentificacion;
    private String nombre;
    private String apellido;
    private LocalDate fechaNacimiento;
    private LocalDate fechaAdmision;
    private String edad;
    private String sexo;
    private String peso;
    private TipoAfiliacionRequest tipoAfiliacion;
    private ProgramaRequest programa;
    private CiudadRequest ciudad;
    private String tipoAdmision;
    private String institucionRemite;
    private DatosAtencionPacienteRequest datosAtencionPaciente;
    private List<Diagnostico> diagnosticos;
}
