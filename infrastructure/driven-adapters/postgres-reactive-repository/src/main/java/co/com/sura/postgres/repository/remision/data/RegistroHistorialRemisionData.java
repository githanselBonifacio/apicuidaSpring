package co.com.sura.postgres.repository.remision.data;

import com.fasterxml.jackson.databind.util.JSONPObject;
import io.r2dbc.postgresql.codec.Json;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "historial_remision")
public class RegistroHistorialRemisionData {
    @Id
    @Column("id_historial")
    private Integer id;

    @Column("id_remision")
    private String idRemision;
    private String estado;
    @Column("fecha_admision")
    private LocalDate fechaAdmision;
    private String programa;
    @Column("tipo_admision")
    private String tipoAdmision;
    @Column("institucion_remite")
    private String institucionRemite;

    @Column("paciente")
    private Json  paciente;

    @Column("ubicacion_paciente")
    private Json  ubicacionPaciente;

    @Column("datos_atencion")
    private Json datosAtencion;

    @Column("diagnosticos")
    private Json diagnosticos;

    @Column("citas")
    private Json citas;
}
