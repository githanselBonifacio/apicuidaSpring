package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class RegistroHistorialRemision {

    private String idRemision;
    private String estado;
    private LocalDateTime fechaAplicacionNovedad;
    private String motivoNovedad;
    private LocalDate fechaAdmision;
    private String programa;
    private String tipoAdmision;
    private String institucionRemite;
    private Object paciente;
    private Object ubicacionPaciente;
    private Object datosAtencion;
    private Object diagnosticos;
    private Object citas;
}
