package co.com.sura.postgres.repository.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;
import javax.persistence.Entity;


@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "tratamientos")
public class TratamientoData {
    @Id
    @Column("id_tratamiento")
    private Integer idTratamiento;

    @Column("tipo_tratamiento")
    private String tipoTratamiento;

    @Column("id_medicamento")
    private String idMedicamento;

    @Column("nombre_medicamento")
    private String nombreMedicamento;

    @Column("presentacion_medicamento")
    private String presentacionMedicamento;

    @Column("codigo_medicamento")
    private String codigoMedicamento;

    @Column("cantidad_dosis")
    private Integer cantidadDosis;

    @Column("unidad_dosis")
    private String unidadDosis;

    @Column("via_administracion")
    private String viaAdministracion;

    private String frecuencia;
    private Integer duracion;

    @Column("no_pbs")
    private Boolean noPBS;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;

    @Column("notificado")
    private boolean notificado;
}
