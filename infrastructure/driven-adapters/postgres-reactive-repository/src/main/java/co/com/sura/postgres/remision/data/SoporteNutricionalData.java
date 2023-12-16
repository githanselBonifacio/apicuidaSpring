package co.com.sura.postgres.remision.data;

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
@Table(name = "soporte_nutricionales")
public class SoporteNutricionalData {

    @Id
    @Column("id_soporte_nutricional")
    private Integer idSoporteNutricional;

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

    private String tipo;

    private String descripcion;

    private Integer duracion;

    private Integer volumen;

    @Column("no_pbs")
    private Boolean noPBS;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
