package co.com.sura.entity.agenda;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class PacienteTratamientoCita {
    private String idRemision;
    private Integer idTratamiento;
    private Integer idSoporteNutricional;
    private String numeroIdentificacion;
    private String tipoIdentificacion;
    private String nombres;
    private String apellidos;
    private String idMedicamento;
    private String nombreMedicamento;
    private String presentacionMedicamento;
    private String codigoMedicamento;
    private Integer cantidadDosis;
    private String unidadDosis;
    private String viaAdministracion;
    private String frecuencia;
    private Integer duracion;
    private Integer volumen;
    private Boolean noPBS;
    private LocalDateTime fechaProgramada;
    private Boolean notificado;
    private String tipo;
}
